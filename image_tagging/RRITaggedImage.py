import numpy as np
from matplotlib import pyplot as plt


class RRITaggedImage:

    def __init__(self, image, processed_image, baseline, contours, peaks):
        self.image = image
        self.processed_image = processed_image,
        self.baseline = baseline
        self.contours = contours
        self.peaks = peaks
        self.x_coords = np.append(contours[:, 0], peaks[:, 0])
        self.y_coords = np.append(contours[:, 1], peaks[:, 1])

    def plot(self):
        """
        Plot filtered contours and identified peaks/troughs over the original
        grayscale image.

        Parameters
        ----------
        empty

        Returns
        -------
        empty
        """

        contours_length = len(self.contours)
        peaks_length = len(self.x_coords) - contours_length

        marker_style = (["."] * contours_length) + \
                       (["+"] * peaks_length)

        marker_size = ([30] * contours_length) + \
                      ([150] * peaks_length)

        marker_color = (["b"] * contours_length) + \
                       (["r"] * peaks_length)

        marker_width = ([0.5] * contours_length) + \
                       ([4] * peaks_length)

        marker_alpha = ([0.75] * contours_length) + \
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

        #plt.show()
