/**
 * The Triangle class is used to model triangles. Like the Square class, it is a
 * subclass of the Shape class and it inherits all the instance variables and
 * methods of the Shape class. The Triangle class overrides the setVertices()
 * method for setting the local coordinates of the 3 vertices of a standard
 * triangle
 * 
 * @author Shaheer Ziya
 */
public class Triangle extends Shape{

  /**
   * <p>
   * A method for setting the local coordinates of the 3 vertices of a standard
   * triangle. Here, a standard triangle is an equilateral triangle having its
   * center located at (0, 0) and one of its vertex on the positive x-axis of its
   * local coordinate system.
   * </p>
   * 
   * <p>
   * For a standard triangle with a distance of d from its center to any of its
   * vertices, the local coordinates of its 3 vertices in counter-clockwise order
   * starting from the one on the positive x-axis are (d, 0), (−𝑑 cos 𝜋/3 ,
   *  −𝑑 sin 𝜋/3), and (−𝑑 cos 𝜋/3 , 𝑑 sin 𝜋/3), respectively.
   * </p>
   * 
   * @param d Specifies the distance from the center of the triangle to any 
   * of its vertices
   */
  public void setVertices(double d) {
    this.xLocal = new double[] {d, (-d * Math.cos(Math.PI / 3)),
      (-d * Math.cos(Math.PI / 3))};
    
    this.yLocal = new double[] {0, (-d * Math.sin(Math.PI / 3)),
      (d * Math.sin(Math.PI / 3))};
    
    } // End of setVertices()
  
} // End of Triangle class
