public class Main {
  public static void main(String[] args) {
    Runtime.printString("Write int:");
    int i = Runtime.readInt();
    Runtime.printString("Echo int:");
    Runtime.printInt(i);
  }
}
