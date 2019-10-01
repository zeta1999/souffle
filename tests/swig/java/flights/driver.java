import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;
import java.util.Set;

public class driver {
  static {
    try {
      System.loadLibrary("SwigInterface");
    } catch (UnsatisfiedLinkError e) {
      System.load(System.getProperty("java.library.path")+ "/" + "libSwigInterface.so");
    }

  }

  public static void main(String argv[]) {
    SWIGSouffleProgram p = SwigInterface.newInstance("flights");
    p.loadAll(".");
    p.run(); 
    p.printAll(".");
    p.finalize();
  }
}
