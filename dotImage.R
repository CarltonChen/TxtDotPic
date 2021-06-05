library(magick)
rm(list= ls())

dotImage = function(file = NULL, px = 20, py = NULL, white = T,
                    dark = 0.5, scratch = F){
  if(is.null(file)){
    errorCondition("Input Path of the image")
  }else{
    B = image_read(file)
    dim0 = dim(B[[1]])[-1]
  }
  
  if(is.null(py)){
    py = round(dim0[1] / dim0[2] * px) * 1.5
  }
  
  px0 = paste0(py * 2, "x", px * 4 + scratch, "!")
  B = image_scale(B, px0)[[1]]
  B = as.integer(B) * ifelse(white, 1, -1)
  
  B1 = apply(B, c(1,2), mean)
  if(scratch) B1 = apply(B1, 2, diff)
  B1 = (B1 - min(B1))/diff(range(B1))
  # image(B1)
  # dim(B1)
  if(white){
    th = quantile(B1, dark)
  }else{
    th = quantile(B1, 1 - dark)
  }
  B1 = B1 > th
  
  x = matrix("a", px, py)
  x0=1:4; y0 = 1:2

  p = matrix(2^c(0, 1, 2, 6, 3, 4, 5, 7), nrow = 4)
  for(i in 1:px){
    for(j in 1:py){
      a = B1[(i - 1) * 4 + x0, (j - 1) * 2 + y0]
      b = sum(as.vector(a) * p)
      if (b == 0){
        b = 10240 + 128
      }else{
        b = 10240 + b
      }
      # b = max(sum(as.vector(a)*p), 1)
      x[i,j] = intToUtf8(b)
    }
  }
  
  x = apply(x, 1, paste0, collapse = "")
  cat(x, sep = "\n")
  return(x)
}

x1 = dotImage(file = "test7.jpg",
              px = 50,
              # py = 50, 
              white = F,
              dark = 0.6,
              scratch = F)

# print(intToUtf8(10240+b))
# for(i in 0:7) print(intToUtf8(10240+2^i)
# p = matrix(2^c(0, 1, 2, 6, 3, 4, 5, 7), nrow = 4)
# a = matrix(c(0,0,
#              0,0,
#              0,0,
#              0,1), nrow = 4, byrow = T)
# b = sum(a*p))
