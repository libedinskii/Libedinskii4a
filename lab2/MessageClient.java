import java.io.*;                // Подключение классов для работы с потоками ввода-вывода (InputStream, OutputStream, IOException и др.)
import java.net.Socket;          // Класс Socket — реализация клиентского TCP-соединения
import java.util.Scanner;        // Класс Scanner — используется для чтения ввода пользователя с клавиатуры

public class MessageClient {     // Объявление публичного класса клиента

    public static void main(String[] args) throws Exception {   // Точка входа в программу

        // Проверка аргументов запуска
        if (args.length != 3) {   // Проверяем, что передано ровно 3 аргумента командной строки
            System.out.println("Usage: send|receive queue host");  // Подсказка по правильному формату запуска
            return;               // Завершение программы при неверном количестве аргументов
        }

        String mode = args[0];     // Первый аргумент — режим работы клиента (send или receive)
        String queue = args[1];    // Второй аргумент — имя очереди сообщений
        String host = args[2];     // Третий аргумент — адрес сервера (например localhost)

        // Создание TCP-соединения
        Socket socket = new Socket(host, 5000);
        // Создаётся клиентский сокет.
        // Выполняется TCP-подключение к серверу по указанному адресу и порту 5000.
        // На этом этапе происходит TCP-handshake (трёхстороннее рукопожатие).

        InputStream in = socket.getInputStream();
        // Получаем входной поток (поток байтов от сервера к клиенту)

        OutputStream out = socket.getOutputStream();
        // Получаем выходной поток (поток байтов от клиента к серверу)

        // Отправка команды установки режима
        out.write((mode + " " + queue + "\n").getBytes());
        // Формируем строку протокола: "send test1\n" или "receive test1\n"
        // Преобразуем строку в массив байтов и отправляем серверу

        out.flush();
        // Принудительно отправляем данные из буфера в сеть
        // Без flush данные могут остаться во внутреннем буфере

        if (mode.equals("send")) {
            // Если режим — отправитель (producer)
            runSender(out);
        } else {
            // Если режим — получатель (consumer)
            runReceiver(in);
        }
    }

    // Логика отправителя (Producer)
    private static void runSender(OutputStream out) throws Exception {

        Scanner scanner = new Scanner(System.in);
        // Создаём Scanner для чтения текста с клавиатуры

        while (true) {   // Бесконечный цикл — позволяет отправлять неограниченное количество сообщений

            System.out.print("Enter message: ");
            // Приглашение пользователю ввести сообщение

            String text = scanner.nextLine();
            // Чтение строки из консоли

            byte[] data = text.getBytes();
            // Преобразование строки в массив байтов
            // TCP работает с байтами, а не со строками

            // Формирование заголовка протокола
            String header = "message " + data.length + "\n";
            // Формируем строку вида: message 5\n
            // где 5 — длина сообщения в байтах

            out.write(header.getBytes());
            // Отправляем заголовок серверу

            out.write(data);
            // Отправляем само тело сообщения

            out.flush();
            // Гарантируем немедленную передачу данных по TCP
        }
    }

    // Логика получателя (Consumer)
    private static void runReceiver(InputStream in) throws Exception {

        while (true) {  // Бесконечный цикл — клиент постоянно ожидает входящие сообщения

            String header = readLine(in);
            // Читаем строку до символа '\n'
            // Это заголовок вида: message 5

            if (header == null) break;
            // Если соединение закрыто — выходим из цикла

            int length = Integer.parseInt(header.split(" ")[1]);
            // Извлекаем длину сообщения из заголовка
            // Разбиваем строку по пробелу и берём второй элемент

            byte[] data = new byte[length];
            // Создаём массив байтов нужной длины для хранения сообщения

            int totalRead = 0;
            // Счётчик уже прочитанных байтов

            while (totalRead < length) {
                // Цикл гарантирует, что мы считаем РОВНО length байт
                // TCP не гарантирует, что всё придёт одним read()

                int read = in.read(data, totalRead, length - totalRead);
                // Читаем данные в массив начиная с позиции totalRead

                totalRead += read;
                // Увеличиваем счётчик прочитанных байтов
            }

            System.out.println("Received: " + new String(data));
            // Преобразуем полученные байты обратно в строку
            // Выводим сообщение пользователю
        }
    }

    // Метод чтения строки из TCP-потока
    private static String readLine(InputStream in) throws IOException {

        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        // Буфер для накопления байтов строки

        int b;
        // Переменная для хранения текущего считанного байта

        while ((b = in.read()) != -1) {
            // Читаем поток по одному байту
            // -1 означает закрытие соединения

            if (b == '\n') break;
            // Если встретили символ конца строки — прекращаем чтение

            buffer.write(b);
            // Добавляем байт в буфер
        }

        if (buffer.size() == 0) return null;
        // Если ничего не прочитано — соединение закрыто

        return buffer.toString();
        // Преобразуем накопленные байты в строку
    }
}
