-module(main).                % объявление модуля main
-export([start/0]).           % экспорт функции start/0 для запуска программы

start() ->
    integral_server:start_link(),
    % запускаем сервер integral_server и регистрируем его локально

    lists:foreach(
        fun(N) ->
            {Time, _} = timer:tc(fun() ->
                gen_server:call(integral_server, {calculate, N})
            end),
            % timer:tc измеряет время выполнения блока кода в микросекундах
            % gen_server:call вызывает сервер синхронно и получает результат интеграла
            % _ — мы игнорируем результат, а замеряем только время

            io:format("~p - ~.4f мс.~n", [N, Time / 1000])
            % выводим номер N и время в миллисекундах с 4 знаками после запятой
        end,
        lists:seq(1, 20)
        % перебираем числа от 1 до 20 (количество потоков N)
    ).
