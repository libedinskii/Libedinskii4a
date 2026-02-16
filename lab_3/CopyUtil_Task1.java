/*
 ============================================================
 Лабораторная работа №3
 Задание №1

 Программа реализует многопоточное копирование данных
 из InputStream в OutputStream.

 Чтение выполняется в отдельном потоке (Producer).
 Запись выполняется в потоке, вызвавшем метод copy() (Consumer).
 Для передачи данных используется BlockingQueue.
 ============================================================
*/

// Импорт класса InputStream — источник байтовых данных
import java.io.InputStream;

// Импорт класса OutputStream — приемник байтовых данных
import java.io.OutputStream;

// Импорт исключения ввода-вывода
import java.io.IOException;

// Импорт конкурентных коллекций (BlockingQueue)
import java.util.concurrent.*;

// Импорт AtomicReference — для безопасной передачи исключений между потоками
import java.util.concurrent.atomic.AtomicReference;

// Объявление класса
public class CopyUtil_Task1 {

    // Статический метод копирования
    public static void copy(final InputStream src,
                            final OutputStream dst) throws IOException {

        // Создаём блокирующую очередь фиксированного размера 64
        // Она будет служить каналом передачи данных между потоками
        final BlockingQueue<byte[]> queue =
                new ArrayBlockingQueue<>(64);

        // Объект для хранения возможного исключения из другого потока
        final AtomicReference<Throwable> exception =
                new AtomicReference<>();

        // Создаём поток-читатель (Producer)
        Thread reader = new Thread(() -> {

            // try-with-resources для автоматического закрытия входного потока
            try (InputStream src0 = src) {

                // Бесконечный цикл чтения
                while (true) {

                    // Создаём новый буфер размером 128 байт
                    byte[] buffer = new byte[128];

                    // Читаем до 127 байт (первый байт зарезервирован)
                    int count = src0.read(buffer, 1, 127);

                    // В нулевой элемент массива записываем количество прочитанных байт
                    buffer[0] = (byte) count;

                    // Помещаем буфер в очередь (если очередь заполнена — поток блокируется)
                    queue.put(buffer);

                    // Если достигнут конец потока (count == -1), выходим
                    if (count == -1) break;
                }

            } catch (Exception e) {

                // Сохраняем исключение для передачи в основной поток
                exception.set(e);
            }

        });

        // Запускаем поток чтения
        reader.start();

        // Запись выполняется в текущем потоке (Consumer)
        try (OutputStream dst0 = dst) {

            // Бесконечный цикл получения данных
            while (true) {

                // Получаем буфер из очереди (если пусто — блокируемся)
                byte[] buffer = queue.take();

                // Если получен маркер завершения — выходим
                if (buffer[0] == -1) break;

                // Записываем данные начиная с 1 индекса
                dst0.write(buffer, 1, buffer[0]);
            }

        } catch (InterruptedException e) {

            // Преобразуем InterruptedException в IOException
            throw new IOException(e);
        }

        // Если в reader-потоке возникла ошибка — пробрасываем её
        if (exception.get() != null)
            throw new IOException(exception.get());
    }
}



