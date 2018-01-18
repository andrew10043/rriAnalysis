from matplotlib import pyplot as plt

"""

=======================================================
Automatic RRI Image Tagging
=======================================================

This simple example demonstrates the basic processing 
of a static image of a renal arterial Doppler waveform. 
The image is first pre-processed using a gaussian
filter and Otsu thresholding, and then subjected to 
contour analysis. Contours are filtered based on pre-
defined criteria to ensure optimal peak/trough 
selection. Finally, contours and peaks/troughs are
plotted overlaying the original grayscale image.

To effectively display the functionality of a key 
component of the contour analysis (assessment of 
waveform location), we have included two images:
one with waveforms above the baseline, and one 
with waveforms below the baseline. In both cases 
the algorithm correctly identifies the location 
and properly filters the contours to allow for 
peak/trough identification.

"""

from image_tagging.RRITagger import RRITagger

tagger = RRITagger()

taggedImage_up = tagger.tag_image(name="images/01_original.jpg")
taggedImage_down = tagger.tag_image(name="images/05_original.jpg")

plt.subplot(121)
taggedImage_up.plot()
plt.subplot(122)
taggedImage_down.plot()

plt.show()