import java.util.TreeSet;

public class P074 {

    public static long chain(long n) {
        TreeSet<Long> ts = new TreeSet<Long>();
        long num = n;
        int len = 0;
        while (ts.add(num) == true) {
            System.out.print( " " + num);
            len++;
            num = P074.digitFact(num);

        }
        System.out.println(len);
        return len;
    }

    void run() {
        int max_n = 1000000;
        TreeSet<Long> ts = new TreeSet<Long>();
        int count = 0;
        long num = 0;
        int len = 0;
        for (int i = 69; i < max_n; i++) {
            ts.clear();
            num = i;
            len = 0;
            while (ts.add(num) == true) {
                len++;
                num = digitFact(num);

            }
            if (len == 60) {
                // System.out.println(i);
                count++;
            }
        }
        System.out.println(count);
    }

    public static long digitFact(long num) {
        long result = 0;
        while (num != 0) {
            result += Factorial(num % 10);
            num /= 10;
        }
        return result;
    }

    public static long Factorial(long l) {
        long fact = 1;
        for (int i = 1; i <= l; i++)
            fact *= i;
        return fact;
    }

    public static void main(String[] args) {
        long t0 = System.currentTimeMillis();
        new P074().run();
        long t1 = System.currentTimeMillis();

        for (long i=0;i<9;i++)
            System.out.println(Factorial(i));

        System.out.println(chain(1479));
        System.out.println((t1 - t0) / 1000 + "s" + (t1 - t0) % 1000 + "ms");
    }
}
