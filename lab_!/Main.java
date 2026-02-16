import java.util.Locale;

//Написать программу, вычисляющую в n = 1..20 потоков интеграл функции f(x) на заданном участке [a,b],
// с заданной шагом h методом трапеций.
//
//Значения: f(x), a, b, h - заданы.
// Программа должна выводить статистику по времени затраченному для вычисления с заданым количеством потоков.

public class Main {

    // Функция, которую интегрируем
    static double f(double x) {
        return Math.sin(x);
    }

    // Класс потока, который считает часть интеграла
    static class Worker extends Thread {

        private final int start;   // начальный индекс шага
        private final int end;     // конечный индекс шага
        private final double a;    // начало интервала
        private final double h;    // шаг интегрирования

        public double partialSum = 0.0; // частичная сумма интеграла

        // Конструктор потока
        Worker(int start, int end, double a, double h) {
            this.start = start;
            this.end = end;
            this.a = a;
            this.h = h;
        }

        // Метод, который выполняется при запуске потока
        @Override
        public void run() {

            // Метод трапеций
            for (int i = start; i < end; i++) {

                double x1 = a + i * h;        // левая точка
                double x2 = a + (i + 1) * h;  // правая точка

                // площадь одной трапеции
                partialSum += (f(x1) + f(x2)) * 0.5 * h;
            }
        }
    }

    // Метод вычисления времени для заданного количества потоков
    static double calculate(double a, double b, double h, int threads)
            throws InterruptedException {

        int n = (int) ((b - a) / h); // количество шагов
        int block = n / threads;     // сколько шагов на поток

        Worker[] workers = new Worker[threads];

        long startTime = System.nanoTime(); // начало измерения

        // Создание и запуск потоков
        for (int i = 0; i < threads; i++) {

            int start = i * block;
            int end = (i == threads - 1) ? n : start + block;

            workers[i] = new Worker(start, end, a, h);
            workers[i].start();
        }

        double totalSum = 0.0;

        // Ожидание завершения потоков
        for (Worker w : workers) {
            w.join();
            totalSum += w.partialSum;
        }

        long endTime = System.nanoTime(); // конец измерения

        return (endTime - startTime) / 1_000_000.0; // в миллисекундах
    }

    public static void main(String[] args) throws InterruptedException {

        // Чтобы числа выводились с точкой
        Locale.setDefault(Locale.US);

        double a = 0.0;
        double b = Math.PI;
        double h = 10E-6;

        // Перебор потоков от 1 до 20
        for (int t = 1; t <= 20; t++) {

            double time = calculate(a, b, h, t);

            // Вывод строго в нужном формате
            System.out.printf("%d - %.4f мс.%n", t, time);
        }
    }
}
