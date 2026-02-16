-module(integral_server).        % объявление модуля с именем integral_server
-behaviour(gen_server).          % указываем, что модуль использует поведение gen_server (OTP сервер)

-export([start_link/0]).         % экспортируем функцию start_link() для запуска сервера
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
                                 % экспортируем стандартные callback-функции gen_server

% API: запуск сервера
start_link() ->
    gen_server:start_link({local, integral_server}, ?MODULE, [], []).
    % создаём и запускаем gen_server
    % {local, integral_server} — регистрируем процесс под именем integral_server
    % ?MODULE — текущий модуль (integral_server)
    % [] — начальное состояние сервера (пустая карта)
    % [] — дополнительные опции (нет)

% callback init
init([]) ->
    {ok, #{}}.
    % инициализация состояния сервера
    % возвращаем кортеж {ok, State}, здесь State — пустая карта #{}

% callback handle_call
handle_call({calculate, N}, _From, State) ->
    Result = calculate(N),
    % если получен запрос {calculate, N}, вычисляем интеграл с этим N
    {reply, Result, State};
    % возвращаем клиенту результат интеграла и текущее состояние сервера

handle_call(_, _, State) ->
    {reply, error, State}.
    % если пришло любое другое сообщение, возвращаем error

% callback handle_cast
handle_cast(_, State) ->
    {noreply, State}.
    % handle_cast обрабатывает асинхронные вызовы
    % в данном случае мы их игнорируем

% callback handle_info
handle_info(_, State) ->
    {noreply, State}.
    % handle_info обрабатывает сообщения, не предназначенные gen_server
    % просто игнорируем

% callback terminate
terminate(_, _) ->
    ok.
    % вызывается при завершении сервера
    % здесь просто возвращаем ok

% callback code_change
code_change(_, State, _) ->
    {ok, State}.
    % вызывается при обновлении кода во время работы
    % возвращаем текущее состояние без изменений

% Основная функция расчёта интеграла
calculate(N) ->
    A = 0,
    B = 1,
    % устанавливаем границы интегрирования: от 0 до 1
    Steps = 1000000,
    % задаём количество разбиений (чем больше, тем точнее)
    H = (B - A) / Steps,
    % вычисляем шаг интегрирования
    integrate(A, B, H, 0, N).
    % вызываем рекурсивную функцию integrate для подсчёта интеграла
    % Acc (аккумулятор) изначально 0

% рекурсивная функция integrate
integrate(X, B, H, Acc, _) when X >= B ->
    Acc.
    % условие выхода рекурсии: если текущая точка >= B, возвращаем накопленную сумму

integrate(X, B, H, Acc, N) ->
    Value = math:sin(N * X),
    % вычисляем значение функции f(x) = sin(N * X) в текущей точке
    integrate(X + H, B, H, Acc + Value * H, N).
    % рекурсивно вызываем integrate для следующего шага
    % прибавляем площадь текущего прямоугольника Value * H к Acc
