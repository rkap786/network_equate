x=rnorm(100, 0 ,1)
y= rbeta(500, 2, 3)
z= runif(200)

## Test x to y
slope_yx= sd(y)/ sd(x)
intercept_yx= mean(y) - slope*mean(x)
xequatedtoy = intercept_yx + slope_yx* x
plot(x, xequatedtoy)


## Test x to z to y
slope_zx= sd(z)/ sd(x)
intercept_zx= mean(z) - slope_zx*mean(x)
xequatedtoz = intercept_zx + slope_zx* x
plot(x,xequatedtoz)

#y to z
slope_zy= sd(z)/ sd(y)
intercept_zy= mean(z) - slope_zy*mean(y)
yequatedtoz = intercept_zy + slope_zy* y
plot(y, yequatedtoz)

### This is same as above
slope_zeqx= sd(xequatedtoz)/ sd(y)
intercept_zeqx= mean(xequatedtoz) - slope_zeqx*mean(y)
yequatedtoxz = intercept_zy + slope_zy* y


# x to y to z
xequatedtoztoy= intercept_zy + xequatedtoy*slope_zy
xequatedtoztoy2= intercept_zeqx + xequatedtoy*slope_zeqx
plot(xequatedtoztoy,xequatedtoztoy2)
abline(0,1)

library(dplyr)
library(ggplot2)
df = bind_cols("X"=x, "XtoY"=xequatedtoy, "XtoZtoY"=xequatedtoztoy  )
plot=df |> 
  pivot_longer(cols = c(XtoY, XtoZtoY), 
               names_to = "equate_link",
               values_to = "equated_score"
               ) |>
  ggplot(aes(X, equated_score,col=equate_link)) + 
  geom_point() + 
  scale_color_manual(values =c("black", "blue")) + theme_minimal() +
  theme(legend.position="bottom") +
  xlab("Observed X score") + ylab("X score linked to Y")

plot
