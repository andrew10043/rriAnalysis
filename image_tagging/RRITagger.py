"""
Class RRITagger

A collection of methods utilized in processing static renal Doppler waveforms
to assess the renal resistive index (RRI).
"""


import cv2
import numpy as np
from analytic_wfm import peakdetect
from image_tagging.RRITaggedImage import RRITaggedImage

class RRITagger:

    def __init__(self):
        self.name = ""

    def tag_image(self, name, lookahead=50, delta=50):
        """
        Fully process a static RRI image.

        Parameters
        ----------
        name : string
            File path to static renal Doppler image.

        lookahead : int | default = 50
            Distance (in pixels) to lookahead when assessing peak/trough
            validity.

        delta : int | default = 50
            Minimum difference between peak/trough and surrounding points to
            be considered a valid peak/trough.

        Returns
        -------
        RRITaggedImage : object of class RRITaggedImage
            Contains original image, processed image,
            baseline value, filtered contours and identified peaks/troughs.
        """
        image = cv2.imread(name, 0)
        baseline = self.find_baseline(image)
        processed_image = self.pre_process(image, baseline)
        contours = self.find_contours(processed_image)
        filtered_contours = self.filter_contours(processed_image,
                                                 contours, baseline)
        peaks = self.find_peaks(filtered_contours, lookahead, delta)

        return RRITaggedImage(image=image, processed_image=processed_image,
                              baseline=baseline,
                              contours=filtered_contours,
                              peaks=peaks)

    def pre_process(self, image, baseline):
        """
        Pre-process grayscale image using the following steps:
        (1) Filter with a gaussian blur. This is done with two different
        kernal sizes.
            (a) The area of the image containing the wave peaks is
            filtered using an 11x11 kernal, while the area containing the wave
            bases is filtered using a 41x41 kernal. This is done to ensure
            removal of contour noise in the wave bases.
            (b) The location at which to switch kernal sizes is determined by
            finding the first row of pixels where the maximum string of
            black pixels is > 100. This identifies the location where waves
            have separated into peaks, with distinct black spaces between them.
        (2) Apply Otsu thresholding.

        Parameters
        ----------
        image : np.array()
            Object returned from call to cv2.imread().

        baseline : float
            Y-coordinate of image baseline. Used here to calculate the
            location at which to switch the gaussian blur kernal size.

        Returns
        -------
        thresh_image : np.array(), values are 0 or 255
            Processed image.
        """

        image_above_bl = image[0:baseline, :]
        image_below_bl = image[baseline:960, :]

        if np.mean(image_above_bl) > np.mean(image_below_bl):
            valid_side = image_above_bl
        else:
            valid_side = image_below_bl

        valid_side_trim = valid_side[:, 30:665]

        # Count consecutive black pixels by row
        consecutive_black = []
        for row in valid_side_trim:
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

            consecutive_black.append(np.max(length))

        if np.mean(image_above_bl) > np.mean(image_below_bl):
            consecutive_black.reverse()
        else:
            pass

        # Find the first row in which the maximum consecutive string of black
        # pixels exceeds 100. This row must occur after the row with the
        # minimum length of consecutive black pixels to ensure the row
        # between the baseline and wave base is not incorrectly selected.
        consecutive_black = np.array(consecutive_black)
        min_black = np.argmin(consecutive_black)
        black_above = np.asarray(np.where(consecutive_black > 100))
        ideal_black_above = black_above[np.where(black_above > min_black)]
        blur_threshold = np.min(ideal_black_above)

        if np.mean(image_above_bl) > np.mean(image_below_bl):
            top_img = image[0:(baseline - blur_threshold), :]
            bottom_img = image[(baseline - blur_threshold):960, :]
            blur_top = cv2.GaussianBlur(top_img, (11, 11), 0)
            blur_bottom = cv2.GaussianBlur(bottom_img, (41, 41), 0)

        else:
            bottom_img = image[(baseline + blur_threshold):960, :]
            top_img = image[0:(baseline + blur_threshold), :]
            blur_top = cv2.GaussianBlur(top_img, (41, 41), 0)
            blur_bottom = cv2.GaussianBlur(bottom_img, (11, 11), 0)

        blur_img = np.vstack((blur_top, blur_bottom))

        ret, thresh_image = cv2.threshold(blur_img, 0, 255,
                                          cv2.THRESH_BINARY + cv2.THRESH_OTSU)

        return thresh_image

    def find_baseline(self, image):
        """
        Identify baseline of the grayscale image using the following steps:
        (1) Utilize Canny edge detection
        (2) Employ the Hough Lines algorithm

        Parameters
        ----------
        image : np.array(), values are 0 or 255
            Processed image.

        Returns
        -------
        baseline : float
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
                                                    cv2.CHAIN_APPROX_SIMPLE)

        return contours

    def filter_contours(self, processed_image, contours, baseline):
        """
        Filter identified contours using the following steps:
        (1) Identify the location of prominent waves (above vs. below baseline).
        (2) Remove contours with length < a certain value to reduce noise.
        (3) Identify ideal buffer from baseline at which to start contour
        filter. This is currently based on the row with the maximum pixel value.
        (4) Remove contours associated with EKG strip at bottom of image.

        Parameters
        ----------
        processed_image : np_array()
            Pre-processed image (gaussian filter + thresholding)

        contours : list
            List of arrays containing coordinates for each contour.

        baseline : float
            Mean y-coordinate of baseline identified by self.find_baseline()

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
        image_below_bl = processed_image[baseline:960, :]

        if np.mean(image_above_bl) > np.mean(image_below_bl):
            valid_side = image_above_bl
        else:
            valid_side = image_below_bl

        # Trim off black margins
        valid_side_trim = valid_side[:, 30:665]

        # Find row with highest pixel value to identify where to filter
        # contours
        mean_intensity = []
        for row in valid_side_trim:
            mean_intensity.append(np.mean(row))
        if np.mean(image_above_bl) > np.mean(image_below_bl):
            mean_intensity.reverse()

        mean_intensity = mean_intensity[20:100]

        bl_threshold = mean_intensity.index(np.max(mean_intensity)) + 20

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
        if np.mean(image_above_bl) > np.mean(image_below_bl):
            filtered_contours = \
                long_contours[long_contours[:, 1] < (baseline - bl_threshold)]
            direction = "up"
        else:
            filtered_contours = \
                long_contours[long_contours[:, 1] > (baseline + bl_threshold)]
            filtered_contours = \
                filtered_contours[filtered_contours[:, 1] < 750]
            direction = "down"

        # Function to filter out redundant contour coordinate pairs
        # If waves are up, removes all but the highest Y-value;
        # If waves are down, removes all but the lowest Y-value
        def grouby_Y(a, dir_waves):
            # Sort by x-coordinate
            b = a[
                a[:, 0].argsort()]
            # Find all unique x-coordinates
            grp_idx = np.flatnonzero(np.r_[True, (b[:-1, 0] != b[1:, 0])])
            # Find max and min Y for all unique x-coordinates
            grp_maxY = np.maximum.reduceat(b[:, 1], grp_idx)
            grp_minY = np.minimum.reduceat(b[:, 1], grp_idx)
            if dir_waves == "up":
                return np.c_[b[grp_idx, 0], grp_maxY]
            elif dir_waves == "down":
                return np.c_[b[grp_idx, 0], grp_minY]

        filtered_contours = grouby_Y(filtered_contours, direction)

        return filtered_contours

    def find_peaks(self, contours, lookahead=50, delta=50):
        """
        Identify peaks and troughs based on filtered contours.

        Parameters
        ----------
        contours : np.array()
            Filtered contours (x-coord, y-coord)

        lookahead : default=50
            Distance to look ahead when assessing the validity of a peak/trough

        delta : default=50
            Minimum difference between peak/trough and points following it to
            qualify as a peak/trough.

        Returns
        -------
        peaks : np.array()
            Array containing coordinates (x-coord, y-coord) of identified
            peaks/troughs
        """

        maxpeaks, minpeaks = peakdetect(y_axis=contours[:, 1],
                                        x_axis=contours[:, 0],
                                        lookahead=lookahead,
                                        delta=delta)
        if bool(maxpeaks):
            maxpeaks = np.vstack(maxpeaks)
            if bool(minpeaks):
                minpeaks = np.vstack(minpeaks)
                peaks = np.vstack((maxpeaks, minpeaks))
            else:
                peaks = maxpeaks
        else:
            if bool(minpeaks):
                minpeaks = np.vstack(minpeaks)
                peaks = minpeaks
            else:
                peaks = None

        return peaks
