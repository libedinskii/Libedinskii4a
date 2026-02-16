/*
 ============================================================
 Лабораторная работа №3
 Задание №3

 Реализована архитектура one-to-many.
 Один поток читает данные.
 Несколько потоков записывают данные.
 Каждый писатель имеет собственную очередь.
 ============================================================
*/

import java.io.*;                 // Потоки ввода-вывода
import java.util.*;               // Списки
import java.util.concurrent.*;    // BlockingQueue
import java.util.concurrent.atomic.AtomicReference;

public class CopyUtil_Task3 {

    public static void copy(final InputStream src,
                            final List<OutputStream> outputs)
            throws IOException {

        // Количество писателей
        int writersCount = outputs.size();

        // Список очередей — по одной на каждого писателя
        List<BlockingQueue<byte[]>> queues = new ArrayList<>();

        // Создаём очереди
        for (int i = 0; i < writersCount; i++)
            queues.add(new ArrayBlockingQueue<>(64));

        // Объект для хранения исключений
        AtomicReference<Throwable> exception =
                new AtomicReference<>();

        // Поток-читатель
        Thread reader = new Thread(() -> {

            try (InputStream src0 = src) {

                while (true) {

                    // Создаём буфер
                    byte[] buffer = new byte[128];

                    // Читаем данные
                    int count = src0.read(buffer, 1, 127);

                    // Записываем длину
                    buffer[0] = (byte) count;

                    // Передаём один и тот же буфер во все очереди
                    for (BlockingQueue<byte[]> q : queues)
                        q.put(buffer);

                    if (count == -1) break;
                }

            } catch (Exception e) {
                exception.set(e);
            }
        });

        reader.start();

        // Список потоков-писателей
        List<Thread> writers = new ArrayList<>();

        // Создаём writer-потоки
        for (int i = 0; i < writersCount; i++) {

            BlockingQueue<byte[]> queue = queues.get(i);
            OutputStream dst = outputs.get(i);

            Thread writer = new Thread(() -> {

                try (OutputStream dst0 = dst) {

                    while (true) {

                        byte[] buffer = queue.take();

                        if (buffer[0] == -1) break;

                        dst0.write(buffer, 1, buffer[0]);
                    }

                } catch (Exception e) {
                    exception.set(e);
                }
            });

            writer.start();
            writers.add(writer);
        }

        // Ожидание завершения всех потоков
        try {
            reader.join();
            for (Thread w : writers)
                w.join();
        } catch (InterruptedException e) {
            throw new IOException(e);
        }

        // Проверка на ошибки
        if (exception.get() != null)
            throw new IOException(exception.get());
    }
}



