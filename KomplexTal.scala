package MandelbrotSet

class Complex(var re: Double, var im: Double){
    def +(c: Complex): Unit = { 
        val tempRe = re 
        val tempIm = im
        re=(tempRe+c.re) 
        im=(tempIm+c.im)
    }
    def -(c: Complex): Unit = {
        val tempRe = re 
        val tempIm = im 
        re=(tempRe-c.re) 
        im=(tempIm-c.im)
    }
    def *(c: Complex): Unit = {
        val tempRe = re 
        val tempIm = im
        re=(tempRe*c.re-tempIm*c.im)
        im=(tempRe*c.im+tempIm*c.re)
    }
    def /(c: Complex): Unit = {
        val tempRe = re 
        val tempIm = im
        re=((tempRe*c.re+tempIm*c.im)/(c.re*c.re+c.im*c.im))
        im=((tempIm*c.re-tempRe*c.im)/(c.re*c.re+c.im*c.im))
    }
    def magni: Int = (scala.math.sqrt((re*re)+(im*im))).toInt
    def theta = scala.math.atan2(re, im)
    override def toString =
        re + (if(im<0)"-"+(-im) else "+"+im) + "i"
}