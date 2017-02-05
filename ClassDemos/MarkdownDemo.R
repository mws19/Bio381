## ------------------------------------------------------------------------
# Use comments profusely in your code!
Time <- seq(1,10) #seq makes integer sequence
print(Time)       #show contents
Resp <- runif(10) # get 10 random uniforms
print(Resp)
plot(x=Time, y=Resp, type="b")

## ---- eval=FALSE---------------------------------------------------------
## plot(runif(20))
## print(seq(1,5))

