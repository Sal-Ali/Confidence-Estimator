gui_bootstrap.location<- function(){
  library(tcltk)
  inputs <- function(){
    x <- tclVar("NOAA[2]")
    y <- tclVar("mean")
    z <- tclVar("10000")
    a <- tclVar(".1")
    tt <- tktoplevel()
    tkwm.title(tt,"Choose parameters for new function                   ")
    x.entry <- tkentry(tt, textvariable=x)
    y.entry <- tkentry(tt, textvariable=y)
    z.entry <- tkentry(tt, textvariable=z)
    a.entry <- tkentry(tt, textvariable=a)
    reset <- function()
    {
      tclvalue(x)<-""
      tclvalue(y)<-""
      tclvalue(z)<-""
      tclvalue(a)<-""
    }
    reset.but <- tkbutton(tt, text="Reset", command=reset)
    submit <- function() {
      x <- tclvalue(x)
      y <- tclvalue(y)
      z <- tclvalue(z)
      a <- tclvalue(a)
      e <- parent.env(environment())
      e$x <- x
      e$y <- y
      e$z <- z
      e$a <- a
      tkdestroy(tt)
    }
    submit.but <- tkbutton(tt, text="start", command=submit)
    tkgrid(tklabel(tt,text="Input raw data"),columnspan=2)
    tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="location function e.g. mean"),columnspan=2)
    tkgrid(tklabel(tt,text="location function"), y.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input number of bootstrap samples e.g. 10000"),columnspan=2)
    tkgrid(tklabel(tt,text="number of bootstrap samples"), z.entry, pady=10, padx =30)
    tkgrid(tklabel(tt,text="Input alpha level for 1-alpha*100% confidence interval"),columnspan=2)
    tkgrid(tklabel(tt,text="alpha"), a.entry, pady=10, padx =30)
    tkgrid(submit.but, reset.but)
    tkwait.window(tt)
    return(c(x,y,z,a))
  }
  # run the function
  predictor_para <- inputs()
  print(predictor_para)
  dat.str<-eval(parse(text=predictor_para[1]))
  location.function<-eval(parse(text=predictor_para[2]))
  nboot<-eval(parse(text=predictor_para[3]))
  alpha<-eval(parse(text=predictor_para[4]))
  my.locbootstrapci.ml(dat.str,location.function,nboot,alpha)
}
