"""
Class RRITaggedImage

Methods utilized to further analyze a tagged image (i.e. plotting).
"""

import numpy as np
from matplotlib import pyplot as plt


class RRITaggedImage:

    def __init__(self, image, masked_image, processed_image, baseline,
                 wave_position, contours, peaks, troughs, bl_threshold,
                 blur_threshold, b):
        self.image = image
        self.masked_image = masked_image
        self.processed_image = processed_image
        self.baseline = baseline
        self.wave_position = wave_position
        self.contours = contours
        self.peaks = peaks
        self.troughs = troughs
        if type(peaks) == np.ndarray:
            self.x_coords = np.append(contours[:, 0], peaks[:, 0])
            self.y_coords = np.append(contours[:, 1], peaks[:, 1])
            if type(troughs) == np.ndarray:
                self.x_coords = np.append(self.x_coords, troughs[:, 0])
                self.y_coords = np.append(self.y_coords, troughs[:, 1])
        elif type(troughs) == np.ndarray:
            self.x_coords = np.append(contours[:, 0], troughs[:, 0])
            self.y_coords = np.append(contours[:, 1], troughs[:, 1])
        else:
            self.x_coords = contours[:, 0]
            self.y_coords = contours[:, 1]
        self.bl_threshold = bl_threshold
        self.blur_threshold = blur_threshold
        self.b = b

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
            peaks_length = len(self.peaks)
        else:
            peaks_length = 0
        if type(self.troughs) == np.ndarray:
            troughs_length = len(self.troughs)
        else:
            troughs_length = 0

        marker_style = (["."] * contours_length) + \
                       (["+"] * (peaks_length + troughs_length))

        marker_size = ([30] * contours_length) + \
                      ([150] * (peaks_length + troughs_length))

        marker_color = (["b"] * contours_length) + \
                       (["r"] * peaks_length) + (["y"] * troughs_length)

        marker_width = ([0.5] * contours_length) + \
                       ([4] * (peaks_length + troughs_length))

        marker_alpha = ([0.5] * contours_length) + \
                       ([1] * (peaks_length + troughs_length))

        plt.imshow(self.image, cmap="gray")
        for _m, c, _x, _y, _z, _l, _a in zip(marker_style, marker_color,
                                             self.x_coords, self.y_coords,
                                             marker_size, marker_width,
                                             marker_alpha):
            plt.scatter(_x, _y, marker=_m, c=c, s=_z,
                        linewidths=_l, alpha=_a)

        plt.axhline(y=self.baseline, color='r', linestyle='-')
        plt.axhline(y=self.bl_threshold, color='b', linestyle='-')
        plt.axhline(y=self.blur_threshold, color='g', linestyle='-')
        plt.xticks([]), plt.yticks([])

    def calc_rri(self):
        """
        Calculates renal resistive index for waves with appropriately
        identified peaks and troughs.

        Parameters
        ----------
        None.

        Returns
        -------
        rri : list
            List of RRI values for each identified wave
        """
        # Calculate mean Y-value of peaks and troughs
        point_mean = np.mean(np.append(self.peaks[:, 1], self.troughs[:, 1]))

        # Remove peaks/troughs if they fall on the incorrect side of the mean
        peaks_less_than = []
        peaks_greater_than = []
        troughs_less_than = []
        troughs_greater_than =[]

        for idx, val in enumerate(self.troughs):
            if val[1] < point_mean:
                troughs_less_than.append(idx)
            elif val[1] > point_mean:
                troughs_greater_than.append(idx)

        for idx, val in enumerate(self.peaks):
            if val[1] < point_mean:
                peaks_less_than.append(idx)
            elif val[1] > point_mean:
                peaks_greater_than.append(idx)

        if self.wave_position == "up":
            peaks = np.delete(self.peaks, peaks_greater_than, axis=0)
            troughs = np.delete(self.troughs, troughs_less_than, axis=0)
        else:
            peaks = np.delete(self.peaks, peaks_less_than, axis=0)
            troughs = np.delete(self.troughs, troughs_greater_than, axis=0)

        # Remove leading troughs, if any
        del_list = []
        for idx, val in enumerate(troughs):
            if val[0] < peaks[0, 0]:
                del_list.append(idx)

        troughs = np.delete(troughs, del_list, axis=0)

        # Remove tailing peaks, if any
        del_list = []
        for idx, val in enumerate(peaks):
            if val[0] > troughs[-1, 0]:
                del_list.append(idx)

        highs = np.delete(peaks, del_list, axis=0)

        # Calculate RRI
        rri = []
        for idx, val in enumerate(highs):
            rri.append((abs(val[1] - self.baseline) -
                        abs(troughs[idx, 1] - self.baseline)) /
                       abs(val[1] - self.baseline))

        return rri


