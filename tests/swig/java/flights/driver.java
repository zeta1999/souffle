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


Map<String, String> dictionary = new HashMap<String, String>();
dictionary.put("key", "value");  

//  SWIGSouffleProgram x = SwigInterface.map_string_string(dictionary);
//  SwigInterface.loadInput(x,"insert_for","edge","path");
    
  }
}
