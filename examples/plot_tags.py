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

# Import: (1) RRITagger Class and (2) plt from matplotlib
from image_tagging.RRITagger import RRITagger
from matplotlib import pyplot as plt

# Define tagger as class RRITagger()
tagger = RRITagger()

# Call tag_image method on two images (one with waves above baseline and one
# with waves below baseline)
taggedImage_up = tagger.tag_image(name="images/01_original.jpg")
taggedImage_down = tagger.tag_image(name="images/05_original.jpg")

# Generate figure with 2x3 row/column structure showing (1) original grayscale
# images; (2) pre-processed images and (3) final tagged imges
fig = plt.figure(figsize=(10,8))

plt.subplot(231)
plt.imshow(taggedImage_up.image, cmap="gray")
plt.title('Original Image - Waves Up')
plt.xticks([]), plt.yticks([])

plt.subplot(232)
plt.imshow(tagger.pre_process(taggedImage_up.image), cmap="gray")
plt.title('Processed Image - Waves Up')
plt.xticks([]), plt.yticks([])

plt.subplot(233)
plt.title('Tagged Image - Waves Up')
taggedImage_up.plot()

plt.subplot(234)
plt.imshow(taggedImage_down.image, cmap="gray")
plt.title('Original Image - Waves Down')
plt.xticks([]), plt.yticks([])

plt.subplot(235)
plt.imshow(tagger.pre_process(taggedImage_down.image), cmap="gray")
plt.title('Processed Image - Waves Down')
plt.xticks([]), plt.yticks([])

plt.subplot(236)
plt.title('Tagged Image - Waves Down')
taggedImage_down.plot()

plt.subplots_adjust(left=0.01, bottom=0.05, right=0.99, top=0.95, wspace=0, hspace=0)
plt.tight_layout()
plt.savefig('plot_tags.png', bbox_inches='tight')
