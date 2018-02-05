"""
Class RRITagger

A collection of methods utilized in processing static renal Doppler waveforms
to assess the renal resistive index (RRI).
"""

import cv2
import numpy as np
from analytic_wfm import peakdetect
from image_tagging.RRITaggedImage import RRITaggedImage, calc_rri


class RRITagger:

    def __init__(self):
        self.name = ""

    def tag_image(self, name, bright_1=100, bright_2=100,
                  blur_threshold=5000000, ahead_sensitivity=0.1,
                  delta_sensitivity=0.5):
        """
        Fully process a static RRI image.

        Parameters
        ----------
        name : str
            File path to static renal Doppler image.

        bright_1: int | default=100
            Parameter in pre_process method.
            Brightness modifier for image processing prior to iterative blur.

        bright_2: int | default=100
            Parameter in pre_process method.
            Brightness modifier for image processing after iterative blur.

        blur_threshold : int | default=5000000
            Parameter in pre_process method.
            Threshold below which iterative blur is stopped. Tested against
            the sum of squared differences between pixels in current iteration
            of blur and previous iteration.

        ahead_sensitivity : float | default=0.1
            Parameter in find_peaks method.
            Proportion of the maximum vertical span of contour points that
            is used as the delta argument for the peak detect function.

        delta_sensitivity : float | default = 0.5
            Parameter in find peaks method.
            Proportion of the number of pixels per wave that is used as the
            lookahead argument for the peak detect function.

        Returns
        -------
        RRITaggedImage : object of class RRITaggedImage

        """
        # Read original image in color
        image = cv2.imread(name, 1)

        # Convert disparate images to uniform size
        if image.shape[0] != 1152 or image.shape[1] != 864:
            image = cv2.resize(src=image, dsize=(1152, 864))

        # Identify baseline and mask image from non-grayscale components
        baseline, gray_mask, masked_image = self.find_baseline(image=image)

        # Find wave position and upper/lower bounds of the true image for
        # later thresholding
        wave_position, upper_bound, lower_bound = self.find_waves(
            image=masked_image, baseline=baseline)

        # Process image, find appropriate coordinates to assess wave frequency
        # and to later filter contours
        processed_image, freq_line, max_threshold = \
            self.pre_process(image=masked_image, baseline=baseline,
                             wave_position=wave_position,
                             upper_bound=upper_bound, lower_bound=lower_bound,
                             bright_1=bright_1, bright_2=bright_2,
                             blur_threshold=blur_threshold)

        # Find contours
        contours = self.find_contours(image=processed_image)

        # Filter contours
        filtered_contours = self.filter_contours(contours=contours,
                                                 baseline=baseline,
                                                 wave_position=wave_position,
                                                 max_threshold=max_threshold)

        # Find peaks and troughs
        peaks, troughs = self.find_peaks(contours=filtered_contours,
                                         baseline=baseline,
                                         wave_position=wave_position,
                                         image=processed_image,
                                         freq_line=freq_line,
                                         ahead_sensitivity=ahead_sensitivity,
                                         delta_sensitivity=delta_sensitivity)

        # Return RRITaggedImage object
        return RRITaggedImage(image=image, masked_image=masked_image,
                              processed_image=processed_image,
                              baseline=baseline,
                              wave_position=wave_position,
                              contours=filtered_contours,
                              peaks=peaks,
                              troughs=troughs,
                              max_threshold=max_threshold,
                              freq_line=freq_line)

    @staticmethod
    def find_baseline(image):
        """
        Identify baseline of the gray scale image using the Hough Lines
        algorithm.

        Parameters
        ----------
        image : array_like
            Color image.

        Returns
        -------
        baseline : int
            Y-coordinate of identified baseline.

        gray_mask: array_like
            Mask to remove all non-gray components.

        gray: array_like
            Masked image with non-gray components removed.
        """
        # Find pixels with R==G==B to create color and gray masks
        bg = image[:, :, 0] == image[:, :, 1]  # B == G
        gr = image[:, :, 1] == image[:, :, 2]  # G == R
        gray_mask = np.bitwise_and(bg, gr, dtype=np.uint8) * 255
        color_mask = 255 - gray_mask
        color_only = cv2.bitwise_and(image, image, mask=color_mask)
        gray_color = cv2.cvtColor(color_only, cv2.COLOR_BGR2GRAY)

        no_color = cv2.bitwise_and(image, image, mask=gray_mask)
        gray = cv2.cvtColor(no_color, cv2.COLOR_BGR2GRAY)

        # Threshold lines
        ret, thresh_image = cv2.threshold(gray_color, 0, 255,
                                          cv2.THRESH_BINARY +
                                          cv2.THRESH_OTSU)

        # Remove small defect on top of many images
        thresh_image[0:10, :] = 0

        # Hough lines algorithm
        lines = cv2.HoughLines(thresh_image, 1, np.pi / 180, 500)
        y1, y2 = 0, 0
        for rho, theta in lines[0]:
            a = np.cos(theta)
            b = np.sin(theta)
            y0 = b * rho
            y1 = int(y0 + 1000 * a)
            y2 = int(y0 - 1000 * a)

        # Return the integer nearest the mean of the two y-values
        # Also return the gray mask and the gray masked image
        return int((y1 + y2) / 2), gray_mask, gray

    @staticmethod
    def mask_image(image, mask):
        """
        Mask non-wave components of the image. Not currently implemented in
        tag_image given performance constraints.

        Parameters
        ----------
        image : array_like
            Original color image.

        mask : array_like
            gray_mask returned from find_baseline method.

        Returns
        -------
        masked_image : array_like
            Gray scale version of image masked for color, text, and icons.
        """
        # Mask out color components
        no_color = cv2.bitwise_and(image, image, mask=mask)

        # Find text regions
        gray = cv2.cvtColor(no_color, cv2.COLOR_BGR2GRAY)
        mser = cv2.MSER_create()
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
            match = \
                cv2.matchShapes(icon_1_contour, contour, 1, 0.0) < 2 or \
                cv2.matchShapes(icon_2_contour, contour, 1, 0.0) < 2
            area = cv2.contourArea(contour) <= 50
            bounds = len(approx) == 4 or len(approx) == 2
            condition = match and area and bounds

            if condition is True:
                cv2.drawContours(cont_mask, [contour], -1, (255, 255, 255), -1)

        cont_mask = 255 - cont_mask

        no_icon = cv2.bitwise_and(no_text, no_text, mask=cont_mask)

        masked_image = cv2.cvtColor(no_icon, cv2.COLOR_BGR2GRAY)

        return masked_image

    @staticmethod
    def find_waves(image, baseline):
        """
        Identify position of waves (above / below the baseline)

        Parameters
        ----------
        image : array_like
            Masked image.

        baseline : int
            Baseline Y-coordinate.

        Returns
        -------
        wave_position : str
            Position of waves ("up" or "down") relative to baseline

        upper_bound : int
            Upper bound of the image as defined by the highest row that is not
            fully black. Allows for proper thresholding.

        lower_bound : int
            Lower bound of the image as defined by the lowest row that is not
            fully black. Allows for proper thresholding.
        """
        # Temporary brighten, blur and threshold to find mask for
        # iterative brightening of waves only
        bright = cv2.convertScaleAbs(image, 1, 7)

        # Define bounds of actual sub-image to ensure proper
        # threshold effect

        not_black = np.asarray(np.where(np.mean(bright, axis=1) != 0))
        if not_black.size == 0:
            lower_bound = 0
            upper_bound = 864
        else:
            lower_bound = np.min(not_black)
            upper_bound = np.max(not_black)

        # Blur and threshold image
        blur_image = cv2.medianBlur(bright[lower_bound:upper_bound, :],
                                    ksize=21)

        ret, thresh_image = cv2.threshold(blur_image, 0, 255,
                                          cv2.THRESH_BINARY +
                                          cv2.THRESH_OTSU)

        # Piece image back to original size
        top = np.zeros((lower_bound, 1152), dtype=np.uint8)
        bottom = np.zeros(((864 - upper_bound), 1152), dtype=np.uint8)

        full = np.vstack((top, thresh_image, bottom))

        # Define and apply waves mask
        waves = full == 255
        waves_mask = np.bitwise_and(waves, waves, dtype=np.uint8) * 255

        # Dilate mask to ensure inclusion of all components of the waves
        dilated_mask = cv2.dilate(waves_mask, None, iterations=10)

        waves_only_img = cv2.bitwise_and(image, image,
                                         mask=dilated_mask)

        # Find portion of waves-only image above and below baseline
        waves_above = waves_only_img[0:baseline, :]
        waves_below = waves_only_img[baseline:864, :]
        above_vec = waves_above[waves_mask[0:baseline, :] == 255]
        below_vec = waves_below[waves_mask[baseline:864, :] == 255]

        # Return up/down based on the sum of pixel values on either side
        # of the baseline
        if len(above_vec) == 0:
            return "down", upper_bound, baseline
        elif len(below_vec) == 0:
            return "up", baseline, lower_bound
        else:
            if np.sum(above_vec) > np.sum(below_vec):
                return "up", baseline, lower_bound
            else:
                return "down", upper_bound, baseline

    @staticmethod
    def pre_process(image, baseline, wave_position, upper_bound, lower_bound,
                    bright_1=60, bright_2=60, blur_threshold=5000000):
        """
        Pre-process gray scale image using a number of iterative processing
        steps. The image is first masked for wave position, and then
        brightened within the wave mask to a specified intensity threshold.
        The brightened image is then iteratively blurred with a median filter
        until the blurring process no longer results in substantial change
        to the image (measured by the sum of squared differences between pixels
        in the image in the current blur iteration and the previous iteration).
        Next the image is again brightened, and Otsu's threshold is applied.

        Parameters
        ----------
        image : array_like
            Object returned mask_image method.

        baseline : int
            Y-coordinate of image baseline. Used here to calculate the
            location at which to switch the gaussian blur kernal size.

        wave_position : str
            Position of waves relative to baseline ("up" or "down")

        upper_bound : int
            Upper bound of the image as defined by the highest row that is not
            fully black. Allows for proper thresholding.

        lower_bound : int
            Lower bound of the image as defined by the lowest row that is not
            fully black. Allows for proper thresholding.

        bright_1 : int | default=60
            Brightness modifier for image processing prior to iterative blur.

        bright_2 : int | default=60
            Brightness modifier for image processing after iterative blur.

        blur_threshold : int | default=5000000
            Threshold below which iterative blur is stopped. Tested against
            the sum of squared differences between pixels in current iteration
            of blur and previous iteration.

        Returns
        -------
        processed_image : array_like
            Processed image.

        freq_line : int
            Y-coordinate of the row at which wave peaks begin to separate.

        max_threshold : int
            Row with the maximal pixel intensity in the processed image.
            Utilized to filter contours.
        """
        # Black out sides of image and area opposite baseline
        image[:, 0:50] = 0
        image[:, 1050:1152] = 0
        if wave_position == "up":
            image[baseline:, :] = 0
        else:
            image[:baseline, :] = 0

        # Temporary brighten, blur and threshold to find mask for
        # iterative brightening of waves only
        bright = cv2.convertScaleAbs(image, 1, 7)

        # Blur and threshold image
        blur_image = cv2.medianBlur(bright[lower_bound:upper_bound, :],
                                    ksize=21)

        ret, thresh_image = cv2.threshold(blur_image, 0, 255,
                                          cv2.THRESH_BINARY +
                                          cv2.THRESH_OTSU)

        # Piece image back to original size
        top = np.zeros((lower_bound, 1152), dtype=np.uint8)
        bottom = np.zeros(((864 - upper_bound), 1152), dtype=np.uint8)

        full = np.vstack((top, thresh_image, bottom))

        # Define and apply waves mask
        waves = full == 255
        waves_mask = np.bitwise_and(waves, waves, dtype=np.uint8) * 255

        # Dilate mask to ensure inclusion of all components of the waves
        dilated_mask = cv2.dilate(waves_mask, None, iterations=10)

        waves_only_img = cv2.bitwise_and(image, image,
                                         mask=dilated_mask)

        # Brighten to mean intensity based on initial value
        non_black = np.squeeze(waves_only_img[np.where(waves_only_img != 0)])
        intensity = np.percentile(non_black, 10)
        bright_adjust = bright_1 / intensity
        bright = cv2.convertScaleAbs(waves_only_img, 1, bright_adjust)

        # Iterative blur + threshold
        thresh = blur_threshold
        count = 0
        prev_blur = cv2.medianBlur(bright, ksize=5)
        blur = cv2.medianBlur(prev_blur, ksize=5)
        ssd_blur = np.sum((blur - prev_blur) ** 2)
        while ssd_blur > thresh:
            blur = cv2.medianBlur(blur, ksize=5)
            ssd_blur = np.sum((blur - prev_blur) ** 2)
            prev_blur = blur
            count += 1

        # Brighten again
        non_black = np.squeeze(blur[np.where(blur != 0)])
        intensity = np.percentile(non_black, 10)
        bright_adjust = bright_2 / intensity
        bright = cv2.convertScaleAbs(blur, 1, bright_adjust)

        # Threshold image
        ret, thresh_image = cv2.threshold(bright[lower_bound:upper_bound, :],
                                          0, 255,
                                          cv2.THRESH_BINARY +
                                          cv2.THRESH_OTSU)

        # Piece image back to original size
        top = np.zeros((lower_bound, 1152), dtype=np.uint8)
        bottom = np.zeros(((864 - upper_bound), 1152), dtype=np.uint8)

        full = np.vstack((top, thresh_image, bottom))

        # Find the row with maximum intensity (this is the contour threshold
        # for later, and also used below. This should be within 200 of baseline.
        max_threshold = np.argmax(np.mean(full[baseline-100:baseline+100, :],
                                          axis=1)) + (baseline-100)

        # Find the non-black row with the median mean pixel intensity to
        # indicate the position at which waves have separated into peaks
        row_means = np.mean(full, axis=1)

        if wave_position == "up":
            valid_means = row_means[:max_threshold]
        else:
            valid_means = row_means[max_threshold:]

        non_black_means = valid_means[np.where(valid_means != 0)]

        median_non_black = np.percentile(non_black_means, 50,
                                         interpolation='nearest')

        freq_line = int(np.max(np.argwhere(row_means == median_non_black)))

        processed_image = full

        return processed_image, freq_line, max_threshold

    @staticmethod
    def find_contours(image):
        """
        Identify contours of pre-processed image.

        Parameters
        ----------
        image : array_like
            Processed image.

        Returns
        -------
        contours : list of arrays
            A list of all contours, stored as n x 2 arrays (x-coord, y-coord)
        """
        im2, contours, hierarchy = cv2.findContours(image,
                                                    cv2.RETR_EXTERNAL,
                                                    cv2.CHAIN_APPROX_NONE)

        return contours

    @staticmethod
    def filter_contours(contours, baseline, wave_position, max_threshold):
        """
        Filter identified contours using the following steps:
        (1) Remove contours with area < 1000.
        (2) Remove contours whose point closest to the baseline is above a
        given threshold.
        (3) Start contour filter at the row with the maximum pixel value;
        removes all contour points below this row.
        (4) Remove contours with identical x-coordinates based on wave
        position. For example, if waves are above the baseline, the coordinate
        pair with the lower Y-coordinate (lower on the image, higher in value)
        is removed.

        Parameters
        ----------
        contours : list
            List of arrays containing coordinates for each contour.

        baseline : int
            Mean y-coordinate of baseline identified by self.find_baseline()

        wave_position : str
            Position of waves relative to baseline ("up" or "down")

        max_threshold : int
            Row with the maximal pixel intensity in the processed image.
            Utilized to filter contours.

        Returns
        -------
        filtered_contours : np.array
            Array of all filtered contours (x-coord, y-coord)
        """
        # Filter for large contours

        contour_area = []
        for val in contours:
            contour_area.append(cv2.contourArea(val))
        contour_area = np.asarray(contour_area)
        contours = np.asarray(contours)

        large = contours[np.where(contour_area > 1000)]

        # Filter contours based on position relative to baseline
        baseline_side = []
        for contour in large:
            if wave_position == "up":
                baseline_side.append(
                    contour[np.argmax(contour[:, :, 1])][0][1])
            else:
                baseline_side.append(
                    contour[np.argmin(contour[:, :, 1])][0][1])

        baseline_side = np.asarray(baseline_side, dtype=int)
        baseline_side = abs(baseline_side - baseline)

        large = large[np.where(baseline_side < 100)]

        large = np.squeeze(
            np.vstack(large)
        )

        large_contours_x = large[:, 0]
        large_contours_y = large[:, 1]

        large_contours = np.column_stack((large_contours_x,
                                          large_contours_y))

        # Set filter threshold
        if wave_position == "up":
            filtered_contours = \
                large_contours[large_contours[:, 1] < max_threshold]
        else:
            filtered_contours = \
                large_contours[large_contours[:, 1] > max_threshold]

        # Function to filter out redundant contour coordinate pairs
        # If waves are up, removes all but the highest Y-value;
        # If waves are down, removes all but the lowest Y-value
        def group_coordinates(a, wave_location):
            # Sort by x-coordinate
            b = a[
                a[:, 0].argsort()]
            # Find all unique x-coordinates
            grp_idx = np.flatnonzero(np.r_[True, (b[:-1, 0] != b[1:, 0])])
            # Find max and min Y for all unique x-coordinates
            grp_max_y = np.maximum.reduceat(b[:, 1], grp_idx)
            grp_min_y = np.minimum.reduceat(b[:, 1], grp_idx)
            if wave_location == "up":
                return np.c_[b[grp_idx, 0], grp_min_y]
            else:
                return np.c_[b[grp_idx, 0], grp_max_y]

        filtered_contours = group_coordinates(filtered_contours, wave_position)

        if wave_position == "up":
            return filtered_contours
        else:
            return filtered_contours

    @staticmethod
    def find_peaks(contours, baseline, wave_position, image,
                   freq_line, ahead_sensitivity=0.1,
                   delta_sensitivity=0.5):
        """
        Identify peaks and troughs based on filtered contours.

        Parameters
        ----------
        contours : array_like
            Filtered contours (x-coord, y-coord)

        baseline : int
            Y-coordinate of baseline.

        wave_position: string
            Position of waves relative to baseline ("up" or "down")

        image : array_like
            Processed image.

        freq_line : int
            Y-coordinate of the row at which wave peaks begin to separate.

        ahead_sensitivity : default=0.2
            Proportion of the maximum vertical span of contour points that
            is used as the delta argument for the peakdetect function.

        delta_sensitivity : default=0.5
            Proportion of span between min contour Y-value and max contour
            Y-value that is given as the delta parameter to the peakdetect
            function.

        Returns
        -------
        peaks : array_like
            Array containing coordinates (x-coord, y-coord) of identified
            peaks

        troughs : array_like
            Array containing coordinates (x-coord, y-coord) of identified
            peaks
        """
        # Set delta based on proportion of the vertical span of contours
        vertical_span = np.max(contours[:, 1]) - np.min(contours[:, 1])

        delta_val = vertical_span * delta_sensitivity

        # Set lookahead based on frequency of waves at the blur threshold
        blur_row = image[freq_line, :]
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
            elif not condition[i] and count > 0:
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
            wave_freq = 400
        else:
            wave_freq = horizontal_span / num_waves

        lookahead_val = int(round(wave_freq * ahead_sensitivity))

        maxpeaks, minpeaks = peakdetect(y_axis=contours[:, 1],
                                        x_axis=contours[:, 0],
                                        lookahead=lookahead_val,
                                        delta=delta_val)

        # If no peaks or troughs found, try different delta sensitivity values
        delta_sens = delta_sensitivity
        while True:
            if bool(maxpeaks) and bool(minpeaks):
                break
            elif delta_sens <= 0.05:
                break
            else:
                delta_sens = delta_sens - 0.05
                delta_val = vertical_span * delta_sens
                maxpeaks, minpeaks = peakdetect(y_axis=contours[:, 1],
                                                x_axis=contours[:, 0],
                                                lookahead=lookahead_val,
                                                delta=delta_val)

        # If no valid RRI, try changing lookahead sensitivity
        ahead_sense = ahead_sensitivity
        while True:
            if bool(maxpeaks):
                maxpeaks = np.vstack(maxpeaks)
            else:
                maxpeaks = None
            if bool(minpeaks):
                minpeaks = np.vstack(minpeaks)
            else:
                minpeaks = None

            if wave_position == "up":
                rri = calc_rri(peaks=minpeaks, troughs=maxpeaks,
                               baseline=baseline, wave_position=wave_position)
            else:
                rri = calc_rri(peaks=maxpeaks, troughs=minpeaks,
                               baseline=baseline, wave_position=wave_position)

            if np.mean(rri) != 0:
                break
            elif ahead_sense <= 0.01:
                break
            else:
                ahead_sense = round(ahead_sense - 0.01, 2)
                lookahead_val = int(round(wave_freq * ahead_sense))
                delta_val = vertical_span * delta_sensitivity
                maxpeaks, minpeaks = peakdetect(y_axis=contours[:, 1],
                                                x_axis=contours[:, 0],
                                                lookahead=lookahead_val,
                                                delta=delta_val)

        if type(maxpeaks) != np.ndarray:
            maxpeaks = None
        if type(minpeaks) != np.ndarray:
            minpeaks = None

        if wave_position == "up":
            peaks = minpeaks
            troughs = maxpeaks
        else:
            peaks = maxpeaks
            troughs = minpeaks

        return peaks, troughs
