import java.util.Scanner;

class runtime {
    // Not supported in Jasmine
    // public static <T> void printInt(T i) {

    public static void printBool(boolean b) {
        System.out.println(b);
    }
    public static void printInt(int i) {
        System.out.println(i);
    }
    public static void printDouble(double d) {
        System.out.println(d);
    }
    public static void printString(String str) {
        System.out.println(str);
    }

    public static int readInt() {
        Scanner scan = new Scanner(System.in);
        int num = scan.nextInt();
        scan.close();
        return num;
    }
    public static double readDouble() {
        Scanner scan = new Scanner(System.in);
        double num = scan.nextDouble();
        scan.close();
        return num;
    }
    public static String readString() {
        Scanner scan = new Scanner(System.in);
        String str = scan.nextLine();
        scan.close();
        return str;
    }
}
