public class Main {

    public static void main(String[] args) {

        long totalSteps = 0;

        for (int i = 1; i <= 1_000_000; ++i) {

            long n = i;

            while (n != 1) {

                if (n % 2 == 0) {
                    n = n / 2;
                } else {
                    n = 3 * n + 1;
                }
                ++totalSteps;
            }
        }
        System.out.print(totalSteps);
    }
}
