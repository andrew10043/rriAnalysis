from RRITagger import *

tagger = RRITagger()

taggedImage = tagger.tag_image(name="images/01_original.jpg")
taggedImage.plot()
