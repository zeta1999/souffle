/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

public class driver {
  static {
    try {
      System.loadLibrary("SwigInterface");
    } catch (UnsatisfiedLinkError e) {
      System.load(System.getProperty("java.library.path") + "/" + "libSwigInterface.so");
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
