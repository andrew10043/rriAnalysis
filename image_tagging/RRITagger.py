"""
Class RRITagger

A collection of methods utilized in processing static renal Doppler waveforms
to assess the renal resistive index (RRI).
"""


import cv2
import numpy as np
from analytic_wfm import peakdetect
from image_tagging.RRITaggedImage import RRITaggedImage

# Use if filtering contour points based on distance from other points
# from scipy.spatial.distance import pdist, squareform

class RRITagger:

    def __init__(self):
        self.name = ""

    def tag_image(self, name, lookahead_sensitivity=0.2,
                  delta_sensitivity=0.5):
        """
        Fully process a static RRI image.

        Parameters
        ----------
        name : string
            File path to static renal Doppler image.

        lookahead_sensitivity : float | default = 0.5
            Proportion of the maximum vertical span of contour points that
            is used as the delta argument for the peakdetect function.

        delta_sensitivity : float | default = 0.2
            Proportion of the number of pixels per wave that is used as the
            lookahead argument for the peakdetect function.

        Returns
        -------
        RRITaggedImage : object of class RRITaggedImage
            Contains original image, masked image, processed image, baseline
            value, wave position (up/down), filtered contours, peak coordinates,
            trough coordinates, baseline threshold for contour filter, and
            blur threshold for processing.
        """
        # Read original image in color
        image = cv2.imread(name, 1)

        # Convert disparate images to uniform size
        if image.shape[0] != 1152 or image.shape[1] != 864:
            image = cv2.resize(src=image, dsize=(1152, 864))

        # Generate gray scale image for identification of baseline
        gray_image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

        # Identify baseline, mask image, find wave position, process image,
        # find contours, filter contours, and find peaks/troughs
        baseline = self.find_baseline(gray_image)
        masked_image = self.mask_image(image)
        wave_position = self.find_waves(masked_image, baseline)
        processed_image, blur_threshold, b = self.pre_process(masked_image,
                                                              baseline,
                                                              wave_position)
        contours = self.find_contours(processed_image)
        filtered_contours, bl_threshold = self.filter_contours(processed_image,
                                                 contours, baseline,
                                                 wave_position)
        peaks, troughs = self.find_peaks(filtered_contours, wave_position,
                                         processed_image, blur_threshold,
                                         lookahead_sensitivity,
                                         delta_sensitivity)

        # Return RRITaggedImage object
        return RRITaggedImage(image=image, masked_image=masked_image,
                              processed_image=processed_image,
                              baseline=baseline,
                              wave_position=wave_position,
                              contours=filtered_contours,
                              peaks=peaks,
                              troughs=troughs,
                              bl_threshold=bl_threshold,
                              blur_threshold=blur_threshold,
                              b=b)

    def find_baseline(self, image):
        """
        Identify baseline of the gray scale image using the following steps:
        (1) Utilize Canny edge detection
        (2) Employ the Hough Lines algorithm

        Parameters
        ----------
        image : np.array(), values are 0 or 255
            Gray scale image.

        Returns
        -------
        baseline : int
            Y-coordinate of identified baseline.
        """
        edges = cv2.Canny(image, 50, 150, apertureSize=3)
        lines = cv2.HoughLines(edges, 1, np.pi / 180, 200)
        for rho, theta in lines[0]:
            a = np.cos(theta)
            b = np.sin(theta)
            x0 = a * rho
            y0 = b * rho
            x1 = int(x0 + 1000 * (-b))
            y1 = int(y0 + 1000 * (a))
            x2 = int(x0 - 1000 * (-b))
            y2 = int(y0 - 1000 * (a))

        return int((y1 + y2) / 2)

    def mask_image(self, image):
        """
        Identify position of waves (above / below the baseline)

        Parameters
        ----------
        image : np.array()
            Original color image.

        Returns
        -------
        masked_image : np.array()
            Gray scale version of image masked for color, text, and icons.
        """

        # Find pixels with R==G==B to create color mask
        bg = image[:, :, 0] == image[:, :, 1]  # B == G
        gr = image[:, :, 1] == image[:, :, 2]  # G == R
        color_mask = np.bitwise_and(bg, gr, dtype=np.uint8) * 255
        no_color = cv2.bitwise_and(image, image, mask=color_mask)

        # Find text regions
        mser = cv2.MSER_create()
        gray = cv2.cvtColor(no_color, cv2.COLOR_BGR2GRAY)
        vis = no_color.copy()
        regions, _ = mser.detectRegions(gray)
        hulls = [cv2.convexHull(p.reshape(-1, 1, 2)) for p in regions]
        cv2.polylines(vis, hulls, 1, (0, 255, 0))
        text_mask = np.zeros((no_color.shape[0], no_color.shape[1], 1),
                             dtype=np.uint8)

        for contour in hulls:
            cv2.drawContours(text_mask, [contour], -1, (255, 255, 255), -1)

        # Filter text mask to exclude wave area to reduce erroneous masking
        text_mask = 255 - text_mask
        text_mask[0:800, 0:1060] = 255

        # Apply text mask
        no_text = cv2.bitwise_and(no_color, no_color, mask=text_mask)

        # Convert to gray scale for further processing
        text_gray = cv2.cvtColor(no_text, cv2.COLOR_BGR2GRAY)

        # Apply thresholding to image and find contours
        ret, text_thresh = cv2.threshold(text_gray, 127, 255, 0)
        im2, contours, hierarchy = cv2.findContours(text_thresh, 2, 1)
        text_contours = contours

        # Read in icon images
        icon1 = cv2.imread("../examples/images/icons/horizontal.png", 0)
        icon2 = cv2.imread("../examples/images/icons/tilted.png", 0)

        # Apply thresholding to icons and find contours
        ret, thresh = cv2.threshold(icon1, 127, 255, 0)
        ret, thresh2 = cv2.threshold(icon2, 127, 255, 0)
        im2, contours, hierarchy = cv2.findContours(thresh, 2, 1)
        icon_1_contour = contours[0]
        im2, contours, hierarchy = cv2.findContours(thresh2, 2, 1)
        icon_2_contour = contours[0]

        # Initialize contour mask
        cont_mask = np.zeros((no_text.shape[0], no_text.shape[1], 1),
                             dtype=np.uint8)

        # Iterate through contours and determine if they are a relative
        # match to the basic icons based on: (1) shape, (2) area, and
        # (3) approximate bounds. If match is reached, add to mask.
        for contour in text_contours:
            epsilon = 0.1 * cv2.arcLength(contour, True)
            approx = cv2.approxPolyDP(contour, epsilon, True)
            match = cv2.matchShapes(icon_1_contour, contour, 1, 0.0) < 2 or \
                    cv2.matchShapes(icon_2_contour, contour, 1, 0.0 < 2)
            area = cv2.contourArea(contour) <= 50
            bounds = len(approx) == 4 or len(approx) == 2
            condition = match and area and bounds

            if condition is True:
                cv2.drawContours(cont_mask, [contour], -1, (255, 255, 255), -1)

        cont_mask = 255 - cont_mask

        no_icon = cv2.bitwise_and(no_text, no_text, mask=cont_mask)

        masked_image = cv2.cvtColor(no_icon, cv2.COLOR_BGR2GRAY)

        return masked_image

    def find_waves(self, masked_image, baseline):
        """
        Identify position of waves (above / below the baseline)

        Parameters
        ----------
        masked_image : np.array()
            Masked image.

        baseline : int
            Baseline Y-coordinate.

        Returns
        -------
        wave_position : string
            Position of waves ("up" or "down") relative to baseline
        """
        if baseline < 105:
            return "down"
        elif baseline > 840:
            return "up"
        else:
            # Temporary brighten, blur and threshold to find mask for
            # iterative brightening of waves only
            bright = cv2.convertScaleAbs(masked_image, 1, 10)

            # Define bounds of actual sub-image to ensure proper thresholding
            not_black = []
            for idx, row in enumerate(bright):
                if np.max(row) != 0:
                    not_black.append(idx)

            if bool(not_black):
                lower_bound = np.min(not_black)
                upper_bound = np.max(not_black)
            else:
                lower_bound = 0
                upper_bound = 864

            blur_image = cv2.GaussianBlur(bright[lower_bound:upper_bound, :],
                                          (91, 91), 0)
            ret, thresh_image = cv2.threshold(blur_image, 0, 255,
                                              cv2.THRESH_BINARY + cv2.THRESH_OTSU)

            top = np.zeros((lower_bound, 1152), dtype=np.uint8)
            bottom = np.zeros(((864 - upper_bound), 1152), dtype=np.uint8)

            full = np.vstack((top, thresh_image, bottom))

            waves = full == 255
            waves_mask = np.bitwise_and(waves, waves, dtype=np.uint8) * 255
            waves_only_img = cv2.bitwise_and(masked_image, masked_image,
                                             mask=waves_mask)

            waves_above = waves_only_img[0:baseline, :]
            waves_below = waves_only_img[baseline:864, :]
            above_vec = waves_above[waves_mask[0:baseline, :] == 255]
            below_vec = waves_below[waves_mask[baseline:864, :] == 255]

            if len(above_vec) == 0:
                return "down"
            elif len(below_vec) == 0:
                return "up"
            else:
                if np.sum(above_vec) > np.sum(below_vec):
                    return "up"
                else:
                    return "down"

    def pre_process(self, masked_image, baseline, wave_position):
        """
        Pre-process grayscale image using a number of iterative processing
        steps. The image is first masked for wave position, and then
        iteratively brightened until pixels within the wave mask reach an
        intensity threshold. The brightened image is then iteratively
        blurred and until there is a consecutive string of white pixels
        greater than a defined value after Otsu's thresholding has been
        applied. Finally, a blur threshold Y-value is determined by finding
        the row of pixels in which there are at least two strings of
        consecutive black pixels >80 pixels in length. This location
        presumably is at the position in which waves have begun to thin
        into their peaks. The portion of the waves below the threshold line
        is blurred using the iterative blur value, while the peaks are
        blurred proportionally less to reduce loss of sharpness when possible.

        Parameters
        ----------
        masked_image : np.array()
            Object returned mask_image method.

        baseline : float
            Y-coordinate of image baseline. Used here to calculate the
            location at which to switch the gaussian blur kernal size.

        wave_position: string
            Position of waves relative to baseline ("up" or "down")

        Returns
        -------
        thresh_image : np.array(), values are 0 or 255
            Processed image.
        """
        # Temporary brighten, blur and threshold to find mask for
        # iterative brightening of waves only
        bright = cv2.convertScaleAbs(masked_image, 1, 10)

        # Define bounds of actual sub-image to ensure proper thresholding
        not_black = []
        for idx, row in enumerate(bright):
            if np.max(row) != 0:
                not_black.append(idx)

        if bool(not_black):
            lower_bound = np.min(not_black)
            upper_bound = np.max(not_black)
        else:
            lower_bound = 0
            upper_bound = 864

        blur_image = cv2.GaussianBlur(bright[lower_bound:upper_bound, :],
                                      (91, 91), 0)
        ret, thresh_image = cv2.threshold(blur_image, 0, 255,
                                          cv2.THRESH_BINARY + cv2.THRESH_OTSU)

        top = np.zeros((lower_bound, 1152), dtype=np.uint8)
        bottom = np.zeros(((864 - upper_bound), 1152), dtype=np.uint8)

        full = np.vstack((top, thresh_image, bottom))

        waves = full == 255
        waves_mask = np.bitwise_and(waves, waves, dtype=np.uint8) * 255
        waves_only_vec = masked_image[waves_mask == 255]
        waves_only_img = cv2.bitwise_and(masked_image, masked_image,
                                         mask=waves_mask)

        # Iterative brighten based on intensity within the waves mask
        intensity = np.mean(waves_only_vec)
        cycle = 0
        bright_waves = waves_only_img
        bright_original = masked_image
        while intensity < 60 and cycle < 20:
            # Increase brightness of image masked for waves
            bright_waves = cv2.convertScaleAbs(bright_waves, 1, 5)

            # Increase brightness of original masked image in parallel
            bright_original = cv2.convertScaleAbs(bright_original, 1, 5)

            # Calculate intensity on image masked for waves
            waves_only_vec = bright_waves[waves_mask == 255]
            intensity = np.mean(waves_only_vec)
            cycle += 1

        # Wave positions
        if wave_position == "up":
            valid_side_ext = masked_image[np.max([lower_bound, 0]):baseline,
                             50:1050]
        else:
            valid_side_ext = masked_image[(baseline + 5):np.min([upper_bound,
                                                                 864]), 50:1050]

        # Find position at which to reduce blur to preserve peaks
        blur_image = cv2.GaussianBlur(bright_original[
                                      lower_bound:upper_bound, :], (91, 91), 0)
        ret, thresh_image = cv2.threshold(blur_image, 0, 255,
                                          cv2.THRESH_BINARY + cv2.THRESH_OTSU)

        full = np.vstack((top, thresh_image, bottom))

        if wave_position == "up":
            valid_ext = full[lower_bound:baseline, 50:1050]
        else:
            valid_ext = full[(baseline + 5):upper_bound, 50:1050]

        # Count consecutive black pixels by row
        consecutive_black = []
        for row in valid_ext:
            length, count = [], 0
            condition = row == 0
            for i in range(len(condition)):
                if condition[i] == True:
                    count += 1
                elif condition[i] == False and count > 0:
                    length.append(count)
                    count = 0

                if i == len(condition) - 1 and count > 0:
                    length.append(count)
                if i == len(condition) - 1 and count == 0:
                    length.append(0)

            long = [elem for elem in length if elem > 80]
            consecutive_black.append(len(long))

        if wave_position == "up":
            consecutive_black.reverse()

        # Find the row with maximum intensity (this is the contour threshold
        # for later, and also used below.

        mean_intensity = []
        for row in valid_side_ext:
            mean_intensity.append(np.mean(row))
        if wave_position == "up":
            mean_intensity.reverse()
            max_threshold = mean_intensity.index(np.max(mean_intensity))
        else:
            max_threshold = mean_intensity.index(np.max(mean_intensity))

        # Find the first row in which there are at least two segments of
        # consecutive black pixels > 50. This row must occur after the row with
        # the maximum string of white pixels (bl_threshold).
        black_above = np.asarray(
            np.where(np.asarray(consecutive_black) >= 2))
        ideal_black_above = black_above[
            np.where(black_above > max_threshold)]

        if ideal_black_above.size == 0:
            location = max_threshold + 50
        else:
            location = np.min(ideal_black_above)

        if wave_position == "up":
            blur_threshold = baseline - location

        else:
            blur_threshold = baseline + 5 + location

        # Iterative blur + threshold
        if wave_position == "up":
            valid_side_trim = bright_original[blur_threshold:baseline,
                              50:1050]
        else:
            valid_side_trim = bright_original[(baseline + 5):blur_threshold,
                              50:1050]

        cycle = 0
        max_white = 0
        b = 5
        while max_white < 400 and cycle < 15:
            blur_trim = cv2.GaussianBlur(valid_side_trim, (b, b), 0)
            ret, thresh_image = cv2.threshold(blur_trim, 0, 255,
                                              cv2.THRESH_BINARY +
                                              cv2.THRESH_OTSU)

            consecutive_white = []
            for row in thresh_image:
                length, count = [], 0
                condition = row == 255
                for i in range(len(condition)):
                    if condition[i] == True:
                        count += 1
                    elif condition[i] == False and count > 0:
                        length.append(count)
                        count = 0

                    if i == len(condition) - 1 and count > 0:
                        length.append(count)
                    if i == len(condition) - 1 and count == 0:
                        length.append(0)

                consecutive_white.append(np.max(length))

            if wave_position == "up":
                consecutive_white.reverse()

            max_white = np.max(consecutive_white)
            cycle += 1
            b += 6

        if b < 47:
            b2 = 11
        else:
            b2 = b - 30

        if wave_position == "up":
            blur_top = cv2.GaussianBlur(bright_original[
                                        lower_bound:blur_threshold, :],
                                        (b2, b2), 0)
            blur_bottom = cv2.GaussianBlur(bright_original[
                                           blur_threshold:upper_bound, ],
                                           (b, b), 0)
        else:
            blur_top = cv2.GaussianBlur(bright_original[
                                        lower_bound:blur_threshold, ],
                                        (b, b), 0)
            blur_bottom = cv2.GaussianBlur(bright_original[
                                           blur_threshold:upper_bound, ],
                                           (b2, b2), 0)

        blur_total = np.vstack((blur_top, blur_bottom))
        ret, thresh_image = cv2.threshold(blur_total, 0, 255,
                                          cv2.THRESH_BINARY +
                                          cv2.THRESH_OTSU)

        processed_image = np.vstack((top, thresh_image, bottom))

        return processed_image, blur_threshold, b

    def find_contours(self, processed_image):
        """
        Identify contours of pre-processed image.

        Parameters
        ----------
        processed_image : np.array(), values are 0 or 255
            Processed image.

        Returns
        -------
        contours : list of arrays
            A list of all contours, stored as n x 2 arrays (x-coord, y-coord)
        """
        im2, contours, hierarchy = cv2.findContours(processed_image,
                                                    cv2.RETR_TREE,
                                                    cv2.CHAIN_APPROX_NONE)

        return contours

    def filter_contours(self, processed_image, contours, baseline,
                        wave_position):
        """
        Filter identified contours using the following steps:
        (1) Remove contours with length < a certain value to reduce noise.
        (2) Identify ideal buffer from baseline at which to start contour
        filter. This is currently based on the row with the maximum pixel value.
        (3) Remove contours with identical x-coordinates based on wave
        position. For example, if waves are above the baseline, the coordinate
        pair with the lower Y-coordinate (lower on the image, higher in value)
        is removed.
        (4) Optionally filter contours based on proximity to other contour
        points to reduce noise.

        Parameters
        ----------
        processed_image : np_array()
            Pre-processed image (gaussian filter + thresholding)

        contours : list
            List of arrays containing coordinates for each contour.

        baseline : float
            Mean y-coordinate of baseline identified by self.find_baseline()

        wave_position: string
            Position of waves relative to baseline ("up" or "down")

        Returns
        -------
        filtered_contours : np.array
            Array of all filtered contours (x-coord, y-coord)
        """
        # Filter out short contours (i.e. noise)
        long = [elem for elem in contours if len(elem) > 100]
        long = np.squeeze(
            np.vstack(long)
        )

        long_contours_x = long[:, 0]
        long_contours_y = long[:, 1]

        long_contours = np.column_stack((long_contours_x,
                                         long_contours_y))

        # Find position of waves relative to baseline
        image_above_bl = processed_image[0:baseline, :]
        image_below_bl = processed_image[baseline:864, :]

        if wave_position == "up":
            valid_side = image_above_bl
        else:
            valid_side = image_below_bl

        # Trim off black margins
        valid_side_trim = valid_side[:, 50:1050]

        # Find row with highest pixel value to identify where to filter
        # contours
        mean_intensity = []
        for row in valid_side_trim:
            mean_intensity.append(np.mean(row))
        if wave_position == "up":
            mean_intensity.reverse()

        mean_intensity = mean_intensity[0:100]

        bl_threshold = mean_intensity.index(np.max(mean_intensity))

        # Alt option for finding bl_threshold - using consecutive black pixels:

        # consecutive_black = []
        # for row in valid_side_trim:
        #     length, count = [], 0
        #     condition = row == 0
        #     for i in range(len(condition)):
        #         if condition[i] == True:
        #             count += 1
        #         elif condition[i] == False and count > 0:
        #             length.append(count)
        #             count = 0
        #
        #         if i == len(condition) - 1 and count > 0:
        #             length.append(count)
        #         if i == len(condition) - 1 and count == 0:
        #             length.append(0)
        #
        #     consecutive_black.append(np.max(length))
        #
        # consecutive_black.reverse()
        #
        # consecutive_black = consecutive_black[20:100]
        # bl_threshold = consecutive_black.index(np.min(consecutive_black)) + 20

        # Set filter threshold
        if wave_position == "up":
            filtered_contours = \
                long_contours[long_contours[:, 1] < (baseline - bl_threshold)]
        else:
            filtered_contours = \
                long_contours[long_contours[:, 1] > (baseline + bl_threshold)]

        # Potential code to filter out contours that are not near other contours
        # dist = squareform(pdist(filtered_contours))
        # close_points = []
        # for row in dist:
        #     close = len(row[np.where(row < 3)])
        #     close_points.append(close > 4)
        #
        # filtered_contours = filtered_contours[close_points]

        # Function to filter out redundant contour coordinate pairs
        # If waves are up, removes all but the highest Y-value;
        # If waves are down, removes all but the lowest Y-value
        def grouby_Y(a, wave_position):
            # Sort by x-coordinate
            b = a[
                a[:, 0].argsort()]
            # Find all unique x-coordinates
            grp_idx = np.flatnonzero(np.r_[True, (b[:-1, 0] != b[1:, 0])])
            # Find max and min Y for all unique x-coordinates
            grp_maxY = np.maximum.reduceat(b[:, 1], grp_idx)
            grp_minY = np.minimum.reduceat(b[:, 1], grp_idx)
            if wave_position == "up":
                return np.c_[b[grp_idx, 0], grp_minY]
            else:
                return np.c_[b[grp_idx, 0], grp_maxY]

        filtered_contours = grouby_Y(filtered_contours, wave_position)

        if wave_position == "up":
            return filtered_contours, baseline-bl_threshold
        else:
            return filtered_contours, baseline+bl_threshold

    def find_peaks(self, contours, wave_position, processed_image,
                   blur_threshold, lookahead_sensitivity=0.2,
                   delta_sensitivity=0.5):
        """
        Identify peaks and troughs based on filtered contours.

        Parameters
        ----------
        contours : np.array()
            Filtered contours (x-coord, y-coord)

        wave_position: string
            Position of waves relative to baseline ("up" or "down")

        processed_image : np.array(), values are 0 or 255
            Processed image.

        blur_threshold: int
            Y-coordinate of the threshold at which blur kernals were switched.
            This provides the location at which waves have presumably begun
            to thin into their peaks. Used here to assess wave frequency.

        lookahead_sensitivity : default=0.2
            Proportion of the maximum vertical span of contour points that
            is used as the delta argument for the peakdetect function.

        delta_sensitivity : default=0.5
            Proportion of span between min contour Y-value and max contour
            Y-value that is given as the delta parameter to the peakdetect
            function.

        Returns
        -------
        peaks : np.array()
            Array containing coordinates (x-coord, y-coord) of identified
            peaks/troughs
        """
        # Set delta based on proportion of the vertical span of contours
        vertical_span = np.max(contours[:, 1]) - np.min(contours[:, 1])

        delta_val = vertical_span * delta_sensitivity

        # Set lookahead based on frequency of waves at the blur threshold
        blur_row = processed_image[blur_threshold, :]

        white_row = np.where(blur_row == 255)
        first_white = np.min(white_row)
        last_white = np.max(white_row)
        horizontal_span = last_white - first_white

        # Count consecutive black pixels in section of horizontal span
        blur_span = blur_row[first_white:last_white]
        condition = blur_span == 0
        length, count = [], 0
        for i in range(len(condition)):
            if condition[i]:
                count += 1
            elif condition[i] == False and count > 0:
                length.append(count)
                count = 0
            if i == len(condition) - 1 and count > 0:
                length.append(count)
            if i == len(condition) - 1 and count == 0:
                length.append(0)

        long = [elem for elem in length if elem > 80]
        num_waves = len(long)

        # Set lookahead_val based on wave frequency
        if num_waves == 0:
            lookahead_val = 50
        else:
            wave_freq = horizontal_span / num_waves
            lookahead_val = int(round(wave_freq * lookahead_sensitivity))

        maxpeaks, minpeaks = peakdetect(y_axis=contours[:, 1],
                                        x_axis=contours[:, 0],
                                        lookahead=lookahead_val,
                                        delta=delta_val)

        if bool(maxpeaks):
            maxpeaks = np.vstack(maxpeaks)
        else:
            maxpeaks = None
        if bool(minpeaks):
            minpeaks = np.vstack(minpeaks)
        else:
            minpeaks = None

        if wave_position == "up":
            peaks = minpeaks
            troughs = maxpeaks
        else:
            peaks = maxpeaks
            troughs = minpeaks

        return peaks, troughs
