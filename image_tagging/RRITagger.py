import cv2
import numpy as np
from analytic_wfm import peakdetect
from image_tagging.RRITaggedImage import RRITaggedImage

class RRITagger:

    def __init__(self):
        self.name = ""

    def tag_image(self, name):
        image = cv2.imread(name, 0)
        baseline = self.find_baseline(image)
        processed_image = self.pre_process(image)
        contours = self.find_contours(processed_image)
        filtered_contours = self.filter_contours(processed_image,
                                                 contours, baseline)
        peaks = self.find_peaks(filtered_contours)

        return RRITaggedImage(image=image, processed_image=processed_image,
                              baseline=baseline,
                              contours=filtered_contours,
                              peaks=peaks)

    def pre_process(self, image):
        """
        Pre-process grayscale image using the following steps:
        (1) Filter with a 5x5 gaussian kernal.
        (2) Apply Otsu thresholding.

        Parameters
        ----------
        image : np.array()
            Object returned from call to cv2.imread().

        Returns
        -------
        thresh_image : np.array(), values are 0 or 255
            Processed image.
        """

        blur_image = cv2.GaussianBlur(image, (5, 5), 0)
        ret, thresh_image = cv2.threshold(blur_image, 0, 255,
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

    def find_contours(self, image):
        """
        Identify contours of pre-processed image.

        Parameters
        ----------
        image : np.array(), values are 0 or 255
            Processed image.

        Returns
        -------
        contours : list of arrays
            A list of all contours, stored as n x 2 arrays (x-coord, y-coord)
        """
        im2, contours, hierarchy = cv2.findContours(image, cv2.RETR_TREE,
                                                    cv2.CHAIN_APPROX_SIMPLE)

        return contours

    def filter_contours(self, processed_image, contours, baseline):
        """
        Filter identified contours using the following steps:
        (1) Identify the location of prominent waves (above vs. below baseline).
        (2) Remove contours on opposite side of baseline + buffer to avoid
        identification of the bottom of waves.
        (3) Remove contours with length < a certain value to reduce noise.
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

        long = [elem for elem in contours if len(elem) > 100]
        long = np.squeeze(
            np.vstack(long)
        )

        long_contours_x = long[:, 0]
        long_contours_y = long[:, 1]

        long_contours = np.column_stack((long_contours_x,
                                         long_contours_y))

        image_above_bl = processed_image[0:baseline, :]
        image_below_bl = processed_image[baseline:960, :]

        if np.mean(image_above_bl) > np.mean(image_below_bl):
            filtered_contours = \
                long_contours[long_contours[:, 1] < baseline - 60]
        else:
            filtered_contours = \
                long_contours[long_contours[:, 1] > baseline + 60]
            filtered_contours = \
                filtered_contours[filtered_contours[:, 1] < 750]

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
        maxpeaks = np.vstack(maxpeaks)
        minpeaks = np.vstack(minpeaks)

        peaks = np.vstack((maxpeaks, minpeaks))

        return peaks
