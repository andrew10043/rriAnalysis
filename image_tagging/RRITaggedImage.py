"""
Class RRITaggedImage

Methods utilized to further analyze a tagged image (i.e. plotting).
"""

import numpy as np
from matplotlib import pyplot as plt


class RRITaggedImage:

    def __init__(self, image, processed_image, baseline, contours, peaks):
        self.image = image
        self.processed_image = processed_image
        self.baseline = baseline
        self.contours = contours
        self.peaks = peaks
        if type(peaks) == np.ndarray:
            self.x_coords = np.append(contours[:, 0], peaks[:, 0])
            self.y_coords = np.append(contours[:, 1], peaks[:, 1])
        else:
            self.x_coords = contours[:, 0]
            self.y_coords = contours[:, 0]

    def plot(self):
        """
        Plot filtered contours and identified peaks/troughs over the original
        grayscale image.

        Parameters
        ----------
        None.

        Returns
        -------
        No direct return; must be followed by plt.show() to display plot if
        desired.
        """

        contours_length = len(self.contours)

        if type(self.peaks) == np.ndarray:
            peaks_length = len(self.x_coords) - contours_length
        else:
            peaks_length = 0

        marker_style = (["."] * contours_length) + \
                       (["+"] * peaks_length)

        marker_size = ([30] * contours_length) + \
                      ([150] * peaks_length)

        marker_color = (["b"] * contours_length) + \
                       (["r"] * peaks_length)

        marker_width = ([0.5] * contours_length) + \
                       ([4] * peaks_length)

        marker_alpha = ([0.5] * contours_length) + \
                       ([1] * peaks_length)

        plt.imshow(self.image, cmap="gray")
        for _m, c, _x, _y, _z, _l, _a in zip(marker_style, marker_color,
                                             self.x_coords, self.y_coords,
                                             marker_size, marker_width,
                                             marker_alpha):
            plt.scatter(_x, _y, marker=_m, c=c, s=_z,
                        linewidths=_l, alpha=_a)

        plt.axhline(y=self.baseline, color='r', linestyle='-')
        plt.xticks([]), plt.yticks([])

    def calc_rri(self):
        point_mean = np.mean(self.peaks[:, 1])

        image_above_bl = self.processed_image[0:self.baseline, :]
        image_below_bl = self.processed_image[self.baseline:960, :]

        if np.mean(image_above_bl) > np.mean(image_below_bl):
            highs = self.peaks[np.where(self.peaks[:, 1] < point_mean)]
            lows = self.peaks[np.where(self.peaks[:, 1] > point_mean)]
        else:
            highs = self.peaks[np.where(self.peaks[:, 1] < point_mean)]
            lows = self.peaks[np.where(self.peaks[:, 1] > point_mean)]

        # Remove leading troughs, if any
        del_list = []
        for idx, val in enumerate(lows):
            if val[0] < highs[0, 0]:
                del_list.append(idx)

        lows = np.delete(lows, del_list, axis=0)

        # Remove tailing peaks, if any
        del_list = []
        for idx, val in enumerate(highs):
            if val[0] > lows[-1, 0]:
                del_list.append(idx)

        highs = np.delete(highs, del_list, axis=0)

        rri = []
        for idx, val in enumerate(highs):
            rri.append((abs(val[1] - self.baseline) -
                        abs(lows[idx, 1] - self.baseline)) /
                       abs(val[1] - self.baseline))

        print(rri)


