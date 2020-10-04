package MandelbrotSet

import java.awt.{Color => JColor}

object Color{
    val black = new JColor(0,0,0)
    val blue = new JColor(0,0,255)
}

object Window{
    import introprog.PixelWindow
    val windowSize = (510,510)
    val window = new PixelWindow(windowSize._1,windowSize._2, "Mandelbrot", Color.blue)
    def complexValue(p: (Int, Int)): Complex = {
    val x: Double = p._1.toDouble
    val y: Double = p._2.toDouble
    new Complex( ((x/100)-3), (-(y/100)+3) )
    }
}

object Math{
    def checkRekursivFormel(c: Complex): Boolean = {
        var cTemp = new Complex(0,0)
        for(i <- 1 to 1000){
            //println(cTemp.magni)
            cTemp=(cTemp*cTemp)+c
        }
        if (cTemp.magni<2){
            return true
        }
        else return false
    }
}


object Main {
    def drawMandelbrot(): Unit = {
        for(y <- 0 to 500){
            for(x <- 0 to 500){
                //println(s"${((x.toDouble/100)-3)} ${(-(y.toDouble/100)+3)}")
                if(Math.checkRekursivFormel(Window.complexValue(x,y))){
                    Window.window.setPixel(x,y,Color.black)
                    }
                }
            }
        }
    def main(args: Array[String]): Unit = {
        drawMandelbrot()
    }
}
