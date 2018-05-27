## Funzione da analizzare

dop.fun <- function(x){
	out = ( sqrt(x*(1-x)) ) * sin( (2.1*pi)/(x+0.05)  )
	return(out)
}

## Plottino della Doppler
curve(dop.fun, from = 0, to = 1, lwd = 5, col = rgb(1,0,.5, 0.4))

curve(dop.fun, from = 0, to = 1, lwd = 5, 
	col = rgb(1,0,.5, 0.4), n = 1001)

curve(dop.fun, from = 0, to = 10, lwd = 5, 
	col = rgb(1,0,.5, 0.4), n = 1001)

curve(dop.fun, from = 0, to = .2, lwd = 5, 
	col = rgb(1,0,.5, 0.4), n = 1001)


## Funzioni base

cos.bas <- function(x,j=3){
	out = sqrt(2)*cos(j*pi*x)
	return(out)
}

## Plottiamole
curve(cos.bas(x,0), from = 0, to = 1, n = 501,
	lwd = 5, col = "blue", ylim = c(-2,2))

curve(cos.bas(x,1), from = 0, to = 1, n = 501,
	lwd = 5, col = "red", add = T)

curve(cos.bas(x,2), from = 0, to = 1, n = 501,
	lwd = 5, col = "green", add = T)

curve(cos.bas(x,10), from = 0, to = 1, n = 501,
	lwd = 5, col = "cyan", add = T)

### Valutiamo numericamente i coefficienti di Fourier per la base di cos()

prod.fun <- function(x,j){
	out = dop.fun(x)*cos.bas(x,j)
	return(out)
}

mu0 = integrate(prod.fun, lower = 0, upper = 1, j = 0)
mu1 = integrate(prod.fun, lower = 0, upper = 1, j = 1)
mu2 = integrate(prod.fun, lower = 0, upper = 1, j = 2)
mu3 = integrate(prod.fun, lower = 0, upper = 1, j = 3)

mu = c()
idx.set = 0:50
for (idx in idx.set){
	mu[idx+1] = integrate(prod.fun, lower = 0, upper = 1, j = idx)$value
}

## plottiamo i coefficienti di Fourier

plot(idx.set, mu, type = "h")


## Ricostruiamo utilizzando i primi J=5 elementi della base

rec.fun <- function(x){
	out = mu[1]*cos.bas(x,0) + mu[2]*cos.bas(x,1) + 
			mu[3]*cos.bas(x,2) + mu[4]*cos.bas(x,3)+
			mu[5]*cos.bas(x,4) + mu[6]*cos.bas(x,5)
	return(out)
}

rec.funHF <- function(x){
	out = mu[1]*cos.bas(x,0) + mu[2]*cos.bas(x,1)+ 
			mu[3]*cos.bas(x,2) + mu[4]*cos.bas(x,3)+
			mu[5]*cos.bas(x,4) + mu[6]*cos.bas(x,5)+
			mu[7]*cos.bas(x,6) + mu[8]*cos.bas(x,7)+ 
			mu[9]*cos.bas(x,8) + mu[10]*cos.bas(x,9)+
			mu[11]*cos.bas(x,10) + mu[12]*cos.bas(x,11)
	return(out)
}

curve(rec.fun, from = 0, to = 1, lwd = 5, col = "purple")
curve(rec.funHF, from = 0, to = 1, lwd = 5, col = "turquoise2", add = TRUE)
curve(dop.fun, from = 0, to = 1, lwd = 6, col = rgb(1,0,0,.3), n=1001,
		add = TRUE)







