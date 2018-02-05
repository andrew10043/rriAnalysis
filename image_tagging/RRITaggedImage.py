"""
Class RRITaggedImage

Methods utilized to further analyze a tagged image (i.e. plotting,
calculation of RRI).
"""

import numpy as np
from matplotlib import pyplot as plt


class RRITaggedImage:

    def __init__(self, image, masked_image, processed_image, baseline,
                 wave_position, contours, peaks, troughs, max_threshold,
                 freq_line):
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
        self.max_threshold = max_threshold
        self.freq_line = freq_line

    def plot(self, plot_freq=False, plot_max=False):
        """
        Plot filtered contours and identified peaks/troughs over the original
        grayscale image.

        Parameters
        ----------
        plot_freq : bool | default=False
            If true, plot will contain a horizontal line at the location where
            wave frequency was determined to calculate the lookahead parameter
            for the peak finding algorithm. Useful for algorithm assessment.

        plot_max : bool | default=False
            If true, plot will contain a horizontal line at the location where
            the maximum intensity of pixels was identified during contour
            filtering. Useful for algorithm assessment.

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

        marker_size = ([10] * contours_length) + \
                      ([150] * (peaks_length + troughs_length))

        marker_color = (["b"] * contours_length) + \
                       (["r"] * peaks_length) + (["y"] * troughs_length)

        marker_width = ([0.5] * contours_length) + \
                       ([4] * (peaks_length + troughs_length))

        marker_alpha = ([0.1] * contours_length) + \
                       ([1] * (peaks_length + troughs_length))

        plt.imshow(self.image, cmap="gray")
        for _m, c, _x, _y, _z, _l, _a in zip(marker_style, marker_color,
                                             self.x_coords, self.y_coords,
                                             marker_size, marker_width,
                                             marker_alpha):
            plt.scatter(_x, _y, marker=_m, c=c, s=_z,
                        linewidths=_l, alpha=_a)

        plt.axhline(y=self.baseline, color='r', linestyle='-',
                    linewidth=0.4)
        if plot_max:
            plt.axhline(y=self.max_threshold, color='b', linestyle='-')
        if plot_freq:
            plt.axhline(y=self.freq_line, color='g', linestyle='-')
        plt.xticks([]), plt.yticks([])


def calc_rri(peaks, troughs, baseline, wave_position):
    """
    Calculates renal resistive index for waves with appropriately
    identified peaks and troughs.

    Parameters
    ----------
    peaks : array_like
        Array of peak coordinates.

    troughs : array_like
        Array of trough coordinates.

    baseline : int
        Y-coordinate of baseline.

    wave_position : str
        Location of waves in relation to the baseline ("up" or "down")

    Returns
    -------
    rri : list
        List of RRI values for each identified wave
    """
    if type(troughs) != np.ndarray or type(peaks) != np.ndarray:
        rri = [0]

    else:
        # Calculate mean Y-value of peaks and troughs
        point_mean = np.mean(np.append(peaks[:, 1], troughs[:, 1]))

        # Remove peaks/troughs if they fall on the wrong side of the mean
        peaks_less_than = []
        peaks_greater_than = []
        troughs_less_than = []
        troughs_greater_than = []

        for idx, val in enumerate(troughs):
            if val[1] < point_mean:
                troughs_less_than.append(idx)
            elif val[1] > point_mean:
                troughs_greater_than.append(idx)

        for idx, val in enumerate(peaks):
            if val[1] < point_mean:
                peaks_less_than.append(idx)
            elif val[1] > point_mean:
                peaks_greater_than.append(idx)

        if wave_position == "up":
            peaks = np.delete(peaks, peaks_greater_than, axis=0)
            troughs = np.delete(troughs, troughs_less_than, axis=0)
        else:
            peaks = np.delete(peaks, peaks_less_than, axis=0)
            troughs = np.delete(troughs, troughs_greater_than, axis=0)

        if troughs.size == 0 or peaks.size == 0:
            rri = [0]
        else:
            # Remove leading troughs, if any
            del_list = []
            for idx, val in enumerate(troughs):
                if val[0] < peaks[0, 0]:
                    del_list.append(idx)

            troughs = np.delete(troughs, del_list, axis=0)

            if troughs.size == 0 or peaks.size == 0:
                rri = [0]
            else:
                # Remove tailing peaks, if any
                del_list = []
                for idx, val in enumerate(peaks):
                    if val[0] > troughs[-1, 0]:
                        del_list.append(idx)

                peaks = np.delete(peaks, del_list, axis=0)

                if peaks.size == 0 or troughs.size == 0:
                    rri = [0]
                else:
                    # Calculate RRI
                    rri = []

                    for idx, val in enumerate(peaks):
                        rri.append((abs(val[1] - baseline) -
                                    abs(troughs[idx, 1] - baseline)) /
                                   abs(val[1] - baseline))

    return rri


