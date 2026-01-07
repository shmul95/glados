public class Main {

    public static void main(String[] args) {
        long result = 0;

        for (long i = 0; i < 1_000_000_000L; i++) {
            result += i;
        }
        System.out.println(result);
    }
}
