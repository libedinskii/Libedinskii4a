/*
 ============================================================
 Лабораторная работа №3
 Задание №2

 Реализован механизм повторного использования буферов.
 Добавлена обратная очередь emptyQueue.
 Это снижает нагрузку на Garbage Collector.
 ============================================================
*/

import java.io.*;                 // Потоки ввода-вывода
import java.util.concurrent.*;    // Конкурентные структуры
import java.util.concurrent.atomic.AtomicReference; // Передача исключений

public class CopyUtil_Task2 {

    public static void copy(final InputStream src,
                            final OutputStream dst) throws IOException {

        // Очередь заполненных буферов (reader → writer)
        final BlockingQueue<byte[]> fullQueue =
                new ArrayBlockingQueue<>(64);

        // Очередь пустых буферов (writer → reader)
        final BlockingQueue<byte[]> emptyQueue =
                new ArrayBlockingQueue<>(64);

        // Объект для хранения исключения
        final AtomicReference<Throwable> exception =
                new AtomicReference<>();

        // Предварительно создаём 64 буфера и кладём их в очередь пустых
        for (int i = 0; i < 64; i++)
            emptyQueue.add(new byte[128]);

        // Поток-читатель
        Thread reader = new Thread(() -> {

            try (InputStream src0 = src) {

                while (true) {

                    // Берём свободный буфер
                    byte[] buffer = emptyQueue.take();

                    // Читаем данные
                    int count = src0.read(buffer, 1, 127);

                    // Записываем длину
                    buffer[0] = (byte) count;

                    // Передаём заполненный буфер писателю
                    fullQueue.put(buffer);

                    if (count == -1) break;
                }

            } catch (Exception e) {
                exception.set(e);
            }

        });

        reader.start();

        // Запись
        try (OutputStream dst0 = dst) {

            while (true) {

                // Получаем заполненный буфер
                byte[] buffer = fullQueue.take();

                if (buffer[0] == -1) break;

                // Записываем данные
                dst0.write(buffer, 1, buffer[0]);

                // Возвращаем буфер обратно в пул
                emptyQueue.put(buffer);
            }

        } catch (InterruptedException e) {
            throw new IOException(e);
        }

        if (exception.get() != null)
            throw new IOException(exception.get());
    }
}



