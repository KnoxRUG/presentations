#####################################################################################
#                       Image processing using R                                    #
#  By Xiaocun Sun, 9/23/2014                                                        #
#  RCS|OIT|UTK, xsun@utk.edu, 865-974-7505                                          #                #
#####################################################################################

###################1.Image processing in R Base#######################################
# R ia a powerful program for data processing and analysis. It also provides a set   #
# of useful tools for image processing                                               #
######################################################################################

##Fundmentals Digital image processing -Gray scale################################
# A digital image is nothing more than data -- numbers indicating variations of     # 
# colors at a particular location on a grid of pixels.                              #
#####################################################################################

getwd()
setwd("C:/Users/xsun/Dropbox/workshop notes/Workshop notes all/R and image processing")

#- slide 6
#example. WHAT YOU SEE IS WHAT YOU GET
D<-matrix(rep(0, 108), 12, 9)
B<-matrix(rep(c(0,0,1),3), 3,3)
C<-t(B)
D[4:6, 1:3]=C
D[7:9, 1:3]=C
D[1:3, 4:6]=B
D[4:6, 4:6]=B
D[7:9, 4:6]=B
D[3,3]=1
par(mfrow=c(1,3))
image(D)
mtext("Image Matrix D")
image(t(D))
mtext("Image transposed Matrix D")
F<-t(D)
image(F[1:9, 4:12])
mtext("Image subset of Matrix D")
par(mfrow=c(2,1))

#- slide 7
##How about details? Shade of Gray and pixel depth
X<-matrix(seq(from = 8, to = 1, by = -1), 1,8)
lab.palette<-colorRampPalette(c("white", "black"), space="Lab") 
image(t(X),col=lab.palette(8), main="Shade of Gray for 3bit gray scale")
X<-matrix(seq(from = 255, to = 0, by = -1), 1,256)
image(t(X),col=lab.palette(256),  main="Shade of Gray for 8bit gray scale")
par(mfrow=c(1,1))

#- slide 8
##############Fundmentals of digital image processing ---color space########################
#RGB is a additive model where the red, green and blue are combined on different quantities#
#RGB is the most used color space                                                          #
#color intensity and color depth                                                           #
############################################################################################

###examples to understand the color space
?colors#information of colors in R
install.packages("scatterplot3d")
library(scatterplot3d)#view of RGB color space in R
?scatterplot3d
## 6 a) The named colors in R, i.e. colors()
cc <- colors()
crgb <- t(col2rgb(cc))
par(xpd = TRUE)
rr <- scatterplot3d(crgb, color = cc, box = TRUE, angle = 20,
                    xlim = c(0, 256), ylim = c(0, 256), zlim = c(0, 256), main="Red_Green_Blue primart color plot")

#########################Limitation of image processing in R base#######################
# manage an image(import, display, export)                                             #
#hard to handle color images in color spaces                                           #
########################################################################################


#- slide 9
#####################R Bioconductor_EBImage############################################
#load EBImage
source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)

#The recommended way of updating all your installed packages is:
?EBImage # imformation about package EBImage

##load image files
myimage= readImage('angle.jpg')
##display image files
display(myimage)
##Write image files
writeImage(myimage, 'm.tiff', quality=100)#Currently supported formats are JPEG, PNG, and TIFF.


#slide 9
##general image processing using EBImage
clown = readImage("clown.png")
display(clown)

print(clown)#request genral info of the image file
dim(clown)#request genral info of the image file
image(clown)#commend in R base, will this work?

clown2 = clown[150:240, 55:110]
clown22 = resize(clown2, 320,200)#zooming
clown3 = clown>0.5
clown4= flip(clown)
clown5= flop(clown)
clown6= rotate(clown,180)
clowncomb1 = combine(clown, clown22,clown3, clown4, clown5,clown6)
display(clowncomb1)
#display combined way
clowncomb2 = combine(clown, clown*2, clown*3, clown*4)
display(clowncomb2)
clowncomb3 = combine(clown, clown*0.5, clown*0.1, clown*0.01)
display(clowncomb3)

#slide 10
####general image processing-color
fc = readImage('FluorescentCells.png')
print(fc)
display(fc)
image(fc)# can we still view the color image in R base?

#split color channels
#option1
colorMode(fc)=Grayscale
display(fc)
#option2
fcg = channel(fc, "asgreen")
fcr = channel(fc, "asred")
fcb = channel(fc, "asblue")
channel_c=combine(fcr, fcg, fcb)
display(channel_c)
##display the split values in each channel
cell_red=rgbImage(red=fcr )
cell_green=rgbImage(green=fcg )
cell_blue=rgbImage(blue=fcb )
cell_re=combine(cell_red, cell_green, cell_blue)
display(cell_re)

#merge and manipulate color channels

#example1. to merge the channels back with adjust on individual channels
cell_color=rgbImage(red=fcr, green=fcg,blue=fcb )
display(cell_color)
cell_color=rgbImage(red=fcg, green=fcb,blue=fcr )
display(cell_color)
cell_color2=rgbImage(red=fcg*4, green=fcb,blue=fcr )
display(cell_color2)
#example2. to merge the channels using different images
nuc = readImage(system.file("images", "nuclei.tif", package="EBImage"))
cel = readImage(system.file("images", "cells.tif", package="EBImage"))
display(nuc)
display(cel)
img = rgbImage(green=cel, blue=nuc)
display(img)

#- slide 11
#more image processing tools
#filters
clownc = readImage('clownc.png')
display(clownc)
##smoothing, using low pass filter
flo = makeBrush(21, shape='disc', step=FALSE)^2
flo = flo/sum(flo)
clown_smooth = filter2(clownc, flo)
display(clown_smooth)

##sharpening, using high pass filter
fhi = matrix(1, nc=3, nr=3)
fhi[2,2] = -8
clown_sharp = filter2(clownc, fhi)
display(clown_sharp)
##blur
clown_blur=gblur(clownc, s=10)
display(clown_blur)
##noise
noise = readImage('noise.jpg')
display(noise)
noise_rm=medianFilter(noise, 1.2)
display(noise_rm)
#contrast---transformation of scale
display(clownc*3)
#brightness------transformation of location
display(clownc+0.3)


#- slide 12
####intensity measurement (application of commonly used R function)
test=fcb[290, 1:512]#green channel of the fluroscence cell image
plot(test, type="l")#plot profile
fcb[290, 1:512]=1
display(fcb)

clownc = readImage('clownc.png')
clownc_data=data.frame(red=as.vector(clownc[, ,1]),green=as.vector(clownc[, ,2]), blue=as.vector(clownc[, ,3]))
attach(clownc_data)
head(clownc_data)
install.packages("lattice")
library(lattice)
cloud(blue~red*green, col=rgb(red,green, blue), pch=19)
boxplot(clownc_data, horizontal=T, col=c("red", "green", "blue"))
attach(clownc_data)
par(mfrow=c(1,3))
hist(red, main="red")
hist(green, main="green")
hist(blue, main="blue")
par(mfrow=c(1,1))

#- slide 13
###segmentation and object processing
##segment nuclei -filling holes
nuc = readImage(system.file("images", "nuclei.tif", package="EBImage"))
display(nuc)
nmask = thresh(nuc, 40, 40, 0.05)
display(nmask)
nmask = fillHull(nmask)
display(nmask)
#count particle numbers after thresholding
clabel = bwlabel(nmask)
cat('Number of objects=', max(clabel),'\n')

#- slide 14
## using paintObjects to highlight objects
nuc = readImage(system.file("images", "nuclei.tif", package="EBImage"))
cel = readImage(system.file("images", "cells.tif", package="EBImage"))
img = rgbImage(green=cel, blue=nuc)
display(cel)
display(img)
res = paintObjects(nmask, img)
display(res)

## make objects
x = readImage(system.file("images", "shapes.png", package="EBImage"))
x = x[110:512,1:130]
display(x)
y = bwlabel(x)
cat('Number of objects=', max(y),'\n')
## remove and reenumerate
y = rmObjects(y, 3)
display(y)

#- slide 15
########group processing

###### What R base can do for group processing
##1. identify files to read in
filesToProcess <- dir(pattern = "mristack.*\\.png$")
filesToProcess

##2. Iterate over each of those file names with lapply
listOfFiles <- lapply(filesToProcess, function(x) readImage(x))

##3. apply manipulation to each file
listOfFiles <- lapply(listOfFiles, function(z) rotate(z, 10))#to rotate
lapply(listOfFiles, function(y) display(y))#to display

display(listOfFiles[[1]])# display one individual image of the group
##save the image files to folder as  png, jpeg, or tiff ect
##4, extract from the list
extract_list<-1:9
for (i in 1:9)
{
  extract_list[i] <- paste("imri10rot_",i, "<-listOfFiles[[",i, "]]",sep = "")  
  
}
print.table <- function(m){write.table(format(m, justify="right"),
                            row.names=F, col.names=F, quote=F)
}
print.table(extract_list)
##5 write to the folder
save_image<-1:9
for (i in 1:9)
{
  save_image[i] <- paste("writeImage(imri10rot_",i,",", "'imri10rot_",i,".png',  quality=100)",sep = "")  
  
}
print.table(save_image)


#- slide 16
###EBImage can do for grouping process
#conbined image will be processed as a group
filesToProcess <- dir(pattern = "mristack.*\\.png$")
filesToProcess
a=readImage(filesToProcess)
print(a)
a_20=rotate(a, 20)
writeImage(a_20, "a_20.png", quality=100)

#- slide 17
##tile and untile, Given a sequence of frames, tile generates a single image with frames tiled
mri=combine(a, a_20)
mri_tile = tile(mri, c(9,2))
display(mri_tile)
## untile
mri_untile=untile(mri_tile, c(9, 2))
display(mri_untile)

#- slide 18

##############################   R package ripa
install.packages("ripa")
library("ripa")

###adjust Brightness
data(logo)
par(mfrow=c(2,2))
plot(logo, main="Source Image")
# the next one is saturated as expected
plot(clipping(2*logo), main="Doubled pixel value with clipping")
plot(clipping(3*logo), main="Doubled pixel value with clipping")
plot(clipping(4*logo), main="Doubled pixel value with clipping")
logo


#- slide 19
####using R and Image together
##Rimagej package
RImageJ
RImageJROI
http://bio7.org

#######end note

a=matrix(rep(0, 168), 7,24)
b2=c(2,3,4,6,7,8,10,11,12,15,16,17,21,22,23)
b3=c(2,4,6,8,11,15,17,23)
b4=c(2,3,4,6,8,11,15,16,17,22,23)
b5=c(4,6,8,11,15,16)
b6=c(2,3,4,6,7,8,11,15,17,18,22)
b7=c(22)
a[2,b2]=1
a[3,b3]=1
a[4,b4]=1
a[5,b5]=1
a[6,b6]=1
a[7,b7]=1
a
b=resize(a,400, 400)
display(rotate(flip(b), 90))

if(condition == TRUE) x <- TRUE
