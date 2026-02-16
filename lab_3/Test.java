import ru.matchtracker.Task1.CopyUtil_Task1;
import ru.matchtracker.Task2.CopyUtil_Task2;
import ru.matchtracker.Task3.CopyUtil_Task3;

// Импортируем классы для работы с потоками ввода-вывода и коллекциями
import java.io.*;                 // InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream, IOException
import java.util.*;               // Random, Arrays, List, Arrays.asList

// Объявление класса теста для всех трёх заданий
public class Test {

    // Главный метод программы
    public static void main(String[] args) throws IOException, InterruptedException {

        // Генерируем тестовые данные размером 64 КБ
        byte[] testData = new byte[64 * 1024];  // массив байтов для копирования

        // Заполняем массив случайными байтами с фиксированным seed для воспроизводимости
        new Random(0).nextBytes(testData);  // теперь testData содержит "случайные" данные

        System.out.println("=== Тест Задание 1 ==="); // заголовок теста

        // Создаём поток для записи результатов Task1
        ByteArrayOutputStream dst1 = new ByteArrayOutputStream();  // выходной поток, куда будет копироваться данные

        // Вызываем метод copy из Task1 (читатель в отдельном потоке, запись в текущем)
        CopyUtil_Task1.copy(new ByteArrayInputStream(testData), dst1);  // передаём входной поток с данными и выходной поток

        // Проверяем, совпадают ли исходные данные с результатом копирования
        if (!Arrays.equals(testData, dst1.toByteArray()))  // сравнение массивов
            throw new AssertionError("Task1 FAILED!");   // если не совпадает — ошибка
        else
            System.out.println("Task1 OK — данные совпадают"); // если совпадает — успех

        System.out.println("=== Тест Задание 2 ==="); // заголовок теста для Task2

        // Создаём поток для записи результатов Task2
        ByteArrayOutputStream dst2 = new ByteArrayOutputStream();  // выходной поток для Task2

        // Вызываем метод copy из Task2 (с повторным использованием буферов)
        CopyUtil_Task2.copy(new ByteArrayInputStream(testData), dst2);  // передаём входной поток и выходной поток

        // Проверка совпадения исходных и скопированных данных
        if (!Arrays.equals(testData, dst2.toByteArray()))
            throw new AssertionError("Task2 FAILED!");   // ошибка, если данные не совпадают
        else
            System.out.println("Task2 OK — данные совпадают"); // успех, данные совпадают

        System.out.println("=== Тест Задание 3 ==="); // заголовок теста для Task3

        // Создаём три потока для записи результатов Task3 (несколько писателей)
        ByteArrayOutputStream dst3_1 = new ByteArrayOutputStream(); // первый писатель
        ByteArrayOutputStream dst3_2 = new ByteArrayOutputStream(); // второй писатель
        ByteArrayOutputStream dst3_3 = new ByteArrayOutputStream(); // третий писатель

        // Объединяем писатели в список для передачи в метод copy
        List<OutputStream> writers = Arrays.asList(dst3_1, dst3_2, dst3_3); // список писателей

        // Вызываем метод copy из Task3 (один читатель → несколько писателей)
        CopyUtil_Task3.copy(new ByteArrayInputStream(testData), writers); // передаём входной поток и список выходных потоков

        // Проверяем каждого писателя на корректность данных
        for (int i = 0; i < writers.size(); i++) {  // перебираем всех писателей
            if (!Arrays.equals(testData, ((ByteArrayOutputStream) writers.get(i)).toByteArray())) // сравниваем данные
                throw new AssertionError("Task3 Writer" + (i+1) + " FAILED!"); // если не совпадает — ошибка
        }

        // Если все писатели получили одинаковые данные
        System.out.println("Task3 OK — все писатели получили одинаковые данные"); // успех

        // Финальное сообщение, что все тесты прошли успешно
        System.out.println("=== Все тесты пройдены успешно! ==="); // вывод финального результата
    }
}


