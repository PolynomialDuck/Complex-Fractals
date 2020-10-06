package MandelbrotSet

import introprog.PixelWindow
import java.awt.{Color => JColor}

object Color{
    var setColor = new JColor(0,0,0)
    var backColor = new JColor(0,0,255)
}


object Window{
    val windowSize = (510,510)
    val window = new PixelWindow(windowSize._1,windowSize._2, "Mandelbrot", Color.backColor)
    def complexValue(p: (Int, Int)): Complex = {
    val x: Double = p._1.toDouble
    val y: Double = p._2.toDouble
    new Complex( ((x/100)-3), (-(y/100)+3) )
    }
    def makeChoice(msg: String = "null", options: Vector[String]): Int = {
        options.indices.foreach(i => println(i+": "+options(i)))
        println(" ")
        val select = io.StdIn.readLine(msg).toInt
        println(" ")
        return select
    }
    def valColor(x: Int = 0): Int = {
        if(0<=x && x<=255){
            return x
        }
        else if(x<0){
            return 0
        }
        else if(x>255){
            return 255
        }
        else return 0
        }
}

object Math{
    var ConstantOriented: Boolean = true
    var cTemp: Complex = new Complex(0,0)
    var constant: Complex = new Complex(0,0)
    def checkRekursivFormel(c: Complex): Boolean = {
        if(ConstantOriented){
            cTemp = new Complex(0,0)
            for(i <- 1 to 1000){
                //println(cTemp.magni)
                cTemp=(cTemp*cTemp)+c
            }
            if (cTemp.magni<2){
                return true
            }
            else return false
        }
        else{
            cTemp = c
            for(i <- 1 to 1000){
                cTemp=cTemp*cTemp+constant
            }
            if (cTemp.magni<2){
                return true
            }
            else return false
        }
    }
}

object Main {
    def drawMandelbrot(): Unit = {
        for(y <- 0 to 500){
            for(x <- 0 to 500){
                if(Math.checkRekursivFormel(Window.complexValue(x,y))){
                    Window.window.setPixel(x,y,Color.setColor)
                    Thread.sleep(0)
                    }
                }
            }
        }
    def main(args: Array[String]): Unit = {
        var tempR: Int = 0
        var tempG: Int = 0
        var tempB: Int = 0
        var tempAns: Int = 0
        var tempRe: Double = 0.0
        var tempIm: Double = 0.0
        drawMandelbrot()
        var quit = false
        while(!quit){
            try{
               tempAns=Window.makeChoice("Pick a choice: ", Vector("Change to Mandelbrotset", "Change to Juliaset", "Change Color of Background", "Change Color of Set","Quit"))
            } catch {
                case e: Exception => println("Error: Couldn't understand the input, try again.")
                tempAns=(-1)
            }
            if(0<=tempAns && tempAns<=4){
                if(tempAns==0){
                    if(Math.ConstantOriented==true){
                        println("You are already viewing the MandelbrotSet!")
                    }
                    else{
                    Math.ConstantOriented = true
                    Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backColor)
                    drawMandelbrot()
                    }
                }
                else if(tempAns==1){
                            println("Enter constant for the JuliaSet")
                            try {
                            tempRe = io.StdIn.readLine("Real Part = ").toDouble
                            } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                tempRe=0
                            }
                            try{
                            tempIm = io.StdIn.readLine("Complex Part = ").toDouble
                            } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                tempIm=0
                            }
                            Math.constant=new Complex(tempRe,tempIm)
                            Math.ConstantOriented = false
                            Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backColor)
                            drawMandelbrot()
                }
                else if(tempAns==2){
                        println("Choose a color using the RGB system...")
                        try{
                        tempR = Window.valColor(io.StdIn.readLine("Red = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                tempR=0
                        }
                        try{
                        tempG = Window.valColor(io.StdIn.readLine("Green = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                tempG=0
                        }
                        try{
                        tempB = Window.valColor(io.StdIn.readLine("Blue = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                tempB=0
                        }
                        Color.backColor = new JColor(tempR, tempG, tempB)
                        Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backColor)
                        drawMandelbrot()
                }
                else if(tempAns==3){
                        println("Choose a color using the RGB system...")
                        try{
                        tempR = Window.valColor(io.StdIn.readLine("Red = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                tempR=0
                        }
                        try{
                        tempG = Window.valColor(io.StdIn.readLine("Green = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                tempG=0
                        }
                        try{
                        tempB = Window.valColor(io.StdIn.readLine("Blue = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                tempB=0
                        }
                        Color.setColor = new JColor(tempR, tempG, tempB)
                        Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backColor)
                        drawMandelbrot()
                    }
                else if(tempAns==4){
                    quit = true
                    Window.window.hide()
                }
                }
                else println("Error: The Input did not represent a choice")
            } 
        }
    }
