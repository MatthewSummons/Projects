import java.awt.Color;

/** The Shape class is used to model general shapes. It has private instance
 * variables for storing color, fill-type, orientation, canvas coordinates of
 * the center, and the local coordinates of the vertices of a shape. It has
 * public getters and setters for accessing its private instance variables. It
 * also has public methods for translating and rotating a shape, and for getting
 * the canvas coordinates of the vertices of a shape.
 * 
 * @author Shaheer Ziya
 */

public class Shape {
  
  // Class Fields //

  /** A Color object specifying the color of the shape. */
  private Color color;
  /** A boolean value specifying whether the shape is filled or not filled. */
  private boolean filled;
  /** A double value specifying the orientation (in radians) of the shape in the screen 
   * coordinate system. */
  private double theta;
  /** A double value specifying the x-coordinate of the center of the shape in the
   * canvas coordinate system. */
  private double xc;
  /** A double value specifying the y-coordinate of the center of the shape in the 
   * canvas coordinate system. */
  private double yc;
  /** An array of double values specifying the x-coordinates of the vertices (in 
   *counter-clockwise order) of the shape in its local coordinate system. */
  private double[] xLocal;
  /** An array of double values specifying the y-coordinates of the vertices (in
   * counter-clockwise order) of the shape in its local coordinate system. */
  private double[] yLocal;

  // Class Methods //

  // No explicit constructor is provided!

  //~~~~~~~~~~~~~~~~~~~~~~~~~~ All getter methods for Shape ~~~~~~~~~~~~~~~~~~~~~~~~~~//
  
  /** A method for retrieving the color of the shape.
   * @return A Color object specifying the color of the shape.
   */ 
  public Color getColor(){
    return this.color;
  };
  
  /** A method for retrieving the fill-type of the shape.
   * @return A boolean value specifying whether the shape is filled or not filled.
   */
  public boolean getFilled(){
    return this.filled;
  };
  
  /** A method for retrieving the orientation (in radians) of the shape in the
   * canvas coordinate system.
   * @return A double value specifying the orientation (in radians) of the shape in the
   * canvas coordinate system.
   */
  public double getTheta(){
    return this.theta;
  };
  
  /** A method for retrieving the orientation (in radians) of the shape in the
   * canvas coordinate system.
   * @return A double value specifying the orientation (in radians) of the shape in the
   * canvas coordinate system.
   */
  public double getXc(){
    return this.xc;
  };
  
  /** A method for retrieving the y-coordinate of the center of the shape in the
   * canvas coordinate system.
   * @return A double value specifying the y-coordinate of the center of the shape in the
   * canvas coordinate system.
   */
  public double getYc(){
    return this.yc;
  };
  
  /** A method for retrieving the x-coordinates of the vertices (in
   * counter-clockwise order) of the shape in its local coordinate system.
   * 
   * @return An array of double values specifying the x-coordinates of the vertices (in
   * counter-clockwise order) of the shape in its local coordinate system.
   */
  public double[] getXLocal(){
    return this.xLocal;
  };
  
  /** A method for retrieving the y-coordinates of the vertices (in
   * counter-clockwise order) of the shape in its local coordinate system.
   * 
   * @return An array of double values specifying the y-coordinates of the vertices (in
   * counter-clockwise order) of the shape in its local coordinate system.
   */
  public double[] getYLocal(){
    return this.yLocal;
  };

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//


  //~~~~~~~~~~~~~~~~~~~~~~~~~~~ All setter methods for Shape ~~~~~~~~~~~~~~~~~~~~~~~~~~//
  
  /** A method for setting the color of the shape. 
   * @param color A Color object specifying the color of the shape.
   */
  public void setColor(Color color){
    this.color = color;
  };

  /** A method for setting the fill-type of the shape.
   * @param filled A boolean value specifying whether the shape is filled or not filled.
   */
  public void setFilled(boolean filled){
    this.filled = filled;
  };
  
  /** A method for setting the orientation of the shape.
   * @param theta A double value specifying the orientation (in radians) of the shape in the
   * canvas coordinate system.
   */
  public void setTheta(double theta){
    this.theta = theta;
  };
  
  /** A method for setting the x-coordinate of the center of the shape in the
   * canvas coordinate system.
   * @param xc A double value specifying the x-coordinate of the center of the shape in the
   * canvas coordinate system.
   */
  public void setXc(double xc){
    this.xc = xc;
  };
  
  /** A method for setting the y-coordinate of the center of the shape in the
   * canvas coordinate system.
   * @param yc A double value specifying the y-coordinate of the center of the shape in the
   * canvas coordinate system.
   */
  public void setYc(double yc){
    this.yc = yc;
  };
  
  /** A method for setting the x-coordinates of the vertices (in counter-clockwise
   * order) of the shape in its local coordinate system.
   * 
   * @param xLocal An array of double values specifying the x-coordinates of the vertices (in
   * counter-clockwise order) of the shape in its local coordinate system.
   */
  public void setXLocal(double[] xLocal){
    this.xLocal = xLocal;
  };
  
  /** A method for setting the y-coordinates of the vertices (in counter-clockwise
   * order) of the shape in its local coordinate system.
   * 
   * @param yLocal An array of double values specifying the y-coordinates of the vertices (in
   * counter-clockwise order) of the shape in its local coordinate system.
   */
  public void setYLocal(double[] yLocal){
    this.yLocal = yLocal;
  };
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
  

  /** A method for translating the center of the shape by dx and dy, respectively,
   * along the x and y directions of the canvas coordinate system.
   * 
   * @param dx A double value specifying the translation along the x direction of the
   * canvas coordinate system.
   * @param dy A double value specifying the translation along the y direction of the
   * canvas coordinate system.
   */
  public void translate(double dx, double dy){
    this.xc += dx;
    this.yc += dy;
  };
  
  /** A method for rotating the shape about its center by an angle of dt (in
   * radians).
   * 
   * @param dTheta A double value specifying the angle (in radians) by which the shape is to be
   * rotated.
   */
  public void rotate(double dTheta){
    this.theta += dTheta;
  };
  
  /** A method for getting the x canvas coordinates of the vertices of the shape
   * (in counter clockwise order rounded to the nearest integer).
   * 
   * We use the following formula to get the canvas coordinates of the vertices
   * from the local coordinates of the vertices
   * x' = (x*cos(𝛉)) - (y*sin(𝛉)) + xc
   * 
   * @return An array of integers containing the x canvas coordinates of the
   *         vertices
   */
  public int[] getX() {
    int arrLength = this.xLocal.length;
    int[] xCanvas = new int[arrLength];

    for (int i = 0; i < this.xLocal.length; i++) {
      // Math.round returns long
      xCanvas[i] = (int) Math.round(((this.getXLocal()[i] * Math.cos(this.getTheta())) -
        (this.getYLocal()[i] * Math.sin(this.getTheta())) + this.getXc()));
    }

    return xCanvas;
  }
  
  /** A method for getting the y canvas coordinates of the vertices of the shape
   * (in counter clockwise order rounded to the nearest integer).
   * 
   * We use the following formula to get the canvas coordinates of the vertices
   * from the local coordinates of the vertices
   * y' = (x*sin(𝛉)) + (y*cos(𝛉)) + yc
   * 
   * @return An array of integers containing the y canvas coordinates of the
   *         vertices
   */
  public int[] getY() {
    int[] yCanvas = new int[this.getYLocal().length];

    for (int i = 0; i < this.getYLocal().length; i++) {
      // Math.round returns long
      yCanvas[i] = (int) Math.round(((this.getXLocal()[i] * Math.sin(this.getTheta())) +
        (this.getYLocal()[i] * Math.cos(this.getTheta())) + this.getYc()));
    }

    return yCanvas;
  }

}
