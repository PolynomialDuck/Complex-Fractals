package MandelbrotSet

import java.awt.{Color => JColor}

object Color{
    val black = new JColor(0,0,0)
    val blue = new Jcolor(0,0,255)
}

object Window{
    import introprog.PixelWindow
    val windowSize(600,600)
    val window = new PixelWindow(windowSize._1,windowSize._2, "Mandelbrot", Color.black)
    def complexValue(p: (Int, Int)): Complex = {
    val c1 = new Complex( ((p._1/100)-3), (-(p._2/100)+3) )
    }
}

object Math{
    def checkRekursivFormel(c: Complex): Boolean = {
        val cTemp = new Complex(0,0)
        for(i <- 1 to 300){
            cTemp = cTemp*cTemp + c
        }
        if (cTemp.magni<2){
            return true
        }
        else return false
    }
}


object Main {
    def drawMandelbrot(): Unit = {
        for(y <- 0 to 600){
            for(x <- 0 to 600){
                if(Math.checkRekursivFormel(Window.complexValue(x,y))){
                    window.fill(x,y,1,1,Color.black)
                    }
                }
            }
        }
    def main(args: Array[String]): Unit = {
        drawMandelbrot()
    }
}