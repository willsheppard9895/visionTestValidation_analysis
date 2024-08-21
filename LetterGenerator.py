import string
from PIL import Image, ImageDraw, ImageFont, ImageOps
import os

# Removes the white border around an image
def crop_white_border(im):
    im2 = ImageOps.invert(im)
    bound_box = im2.getbbox()
    if bound_box:
        return im.crop(bound_box)

# Creates an image of a single letter
def generate_letter_image(letter, filename, image_width, transparent = False, contrast_perc = 100):
  image = Image.new('RGBA', (image_width, image_width), color = (255, 255, 255, 0 if transparent else 255))


  # load the font and set its size
  font = ImageFont.truetype('fonts/arial.ttf', int(image_width))

  # Set up the text and centre it
  d = ImageDraw.Draw(image)
  text_size = d.textsize(letter, font)
  text_pos = (int((image_width - text_size[0])/2),int((image_width - text_size[1])/2))
  
  # set opacity of entire image
  alpha = int(255 * (contrast_perc/100))

  # Save the image
  d.text(text_pos, letter, font = font, fill=(0,0,0))
  image.putalpha(alpha)
  image = image.convert("RGB")
  image = crop_white_border(image)
  image.save('./' + filename + ".png")

# Creates an image of every lowercase and uppercase letter
def generate_all_letters(image_width, transparent, contrast_perc = 100):

  
  #os.mkdir(str(contrast_perc)+ "_" + str(transparent))
  #os.mkdir("UPPER_"+ str(transparent))

  #for letter in string.ascii_lowercase:
  #  generate_letter_image(letter, letter+"_lower_"+str(image_width)+"_"+str(contrast_perc), image_width, transparent, contrast_perc)
  for letter in string.ascii_uppercase:
    generate_letter_image(letter, letter+"_upper_"+str(image_width)+"_"+str(contrast_perc)+"_"+str(transparent), image_width, transparent, contrast_perc)

# 300 is image size. Change the "False" to True for transparent backgrounds. Change final number to alter contrast of the image.
range = range(1, 101)
#for x in range:
#      generate_all_letters(300, False, x)
      
generate_all_letters(300, False, 100)
