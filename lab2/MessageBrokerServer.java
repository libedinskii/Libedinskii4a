import java.io.*;                  // Подключение классов для работы с потоками ввода/вывода (InputStream, OutputStream и т.д.)
import java.net.*;                 // Подключение сетевых классов (ServerSocket, Socket)
import java.util.concurrent.*;     // Подключение утилит для многопоточности и потокобезопасных коллекций

public class MessageBrokerServer { // Главный класс сервера брокера сообщений

    // TCP порт, на котором сервер принимает входящие соединения
    private static final int PORT = 5000;

    // Глобальное хранилище очередей сообщений
    // ConcurrentHashMap — потокобезопасная ассоциативная коллекция
    // Ключ (String) — имя логической очереди
    // Значение — BlockingQueue<byte[]> (очередь сообщений в виде массива байт)
    private static final ConcurrentHashMap<String, BlockingQueue<byte[]>> queues =
            new ConcurrentHashMap<>();

    public static void main(String[] args) throws IOException {

        // Создание серверного сокета — точка входа TCP-соединений
        ServerSocket serverSocket = new ServerSocket(PORT);

        // Логирование запуска сервера
        System.out.println("Broker started on port " + PORT);

        // Бесконечный цикл — сервер работает в режиме постоянного прослушивания
        while (true) {

            // Метод accept() блокируется до тех пор,
            // пока клиент не установит TCP-соединение
            Socket socket = serverSocket.accept();

            // Для каждого клиента создаётся отдельный поток исполнения
            // Это обеспечивает параллельную обработку подключений
            new Thread(new ClientHandler(socket)).start();
        }
    }

    // Внутренний класс обработки одного клиента
    // Реализует интерфейс Runnable для запуска в отдельном потоке
    static class ClientHandler implements Runnable {

        // TCP-сокет конкретного клиента
        private final Socket socket;

        // Конструктор сохраняет клиентский сокет
        ClientHandler(Socket socket) {
            this.socket = socket;
        }

        // Метод run() — точка входа нового потока
        @Override
        public void run() {
            try {

                // Получение входного потока (для чтения данных от клиента)
                InputStream in = socket.getInputStream();

                // Получение выходного потока (для отправки данных клиенту)
                OutputStream out = socket.getOutputStream();

                // Чтение первой строки — инициализация протокола
                // Клиент сообщает режим работы: send или receive
                String firstLine = readLine(in);

                // Разделение строки по пробелу
                String[] parts = firstLine.split(" ");

                // Первый параметр — режим работы (Producer или Consumer)
                String mode = parts[0];

                // Второй параметр — имя очереди
                String queueName = parts[1];

                // Если очередь с таким именем отсутствует —
                // создаём новую потокобезопасную очередь LinkedBlockingQueue
                queues.putIfAbsent(queueName, new LinkedBlockingQueue<>());

                // Получаем ссылку на конкретную очередь сообщений
                BlockingQueue<byte[]> queue = queues.get(queueName);

                // Если клиент работает как Producer
                if (mode.equals("send")) {

                    // Передаём управление в обработчик отправителя
                    handleSender(in, queue);

                } else {

                    // Иначе клиент работает как Consumer
                    handleReceiver(out, queue);
                }

            } catch (Exception e) {

                // Обработка ситуации разрыва соединения
                System.out.println("Client disconnected");
            }
        }

        // Метод обработки отправителя (Producer)
        private void handleSender(InputStream in,
                                  BlockingQueue<byte[]> queue) throws Exception {

            // Бесконечный цикл приёма сообщений
            while (true) {

                // Чтение заголовка формата: "message N"
                String header = readLine(in);

                // Если соединение закрыто — выход из цикла
                if (header == null) break;

                // Извлечение длины сообщения (N байт)
                int length = Integer.parseInt(header.split(" ")[1]);

                // Создание буфера фиксированного размера
                byte[] data = new byte[length];

                // Счётчик фактически прочитанных байт
                int totalRead = 0;

                // TCP не гарантирует, что все байты придут за одно чтение
                // Поэтому реализован цикл дочитывания
                while (totalRead < length) {

                    // Чтение части данных в буфер
                    int read = in.read(data, totalRead, length - totalRead);

                    // Увеличение счётчика прочитанных байт
                    totalRead += read;
                }

                // Добавление сообщения в блокирующую очередь
                // Метод put() потокобезопасен
                queue.put(data);

                // Логирование события
                System.out.println("Message added to queue");
            }
        }

        // Метод обработки получателя (Consumer)
        private void handleReceiver(OutputStream out,
                                    BlockingQueue<byte[]> queue) throws Exception {

            // Бесконечный цикл отправки сообщений
            while (true) {

                // Метод take() блокируется,
                // если очередь пуста (реализация механизма ожидания)
                byte[] message = queue.take();

                // Формирование заголовка согласно протоколу
                String header = "message " + message.length + "\n";

                // Отправка заголовка клиенту
                out.write(header.getBytes());

                // Отправка тела сообщения
                out.write(message);

                // Принудительная отправка буфера в сеть
                out.flush();

                // Логирование доставки сообщения
                System.out.println("Message delivered to receiver");
            }
        }

        // Вспомогательный метод чтения строки до символа '\n'
        private String readLine(InputStream in) throws IOException {

            // Буфер динамического размера
            ByteArrayOutputStream buffer = new ByteArrayOutputStream();

            int b;

            // Чтение по одному байту
            while ((b = in.read()) != -1) {

                // Если достигнут символ новой строки — завершение чтения
                if (b == '\n') break;

                // Запись байта во временный буфер
                buffer.write(b);
            }

            // Если ничего не прочитано — соединение закрыто
            if (buffer.size() == 0) return null;

            // Преобразование массива байт в строку
            return buffer.toString();
        }
    }
}
