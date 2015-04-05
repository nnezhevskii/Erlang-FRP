# Erlang-FRP
Легковесная FRP библиотека на языке Erlang.

## Реализация

#### Реализация узлов
Узел f_node определяет идентификатор обрабатывающего процесса и callback функцию.

Обрабатывающий процесс:
- Логика процесса описана в node:loop/2
- Хранит информацию об обрабатываемом узле и текущее значение (состояние).
- Осуществляет приём событий, пересчитывает значение и пересылает обновленное событие всем зависящим узлам

Создание узла:
```
{ok, Node} = frp_api:start_node(CustomHandler, StartingState).

```
Callback функция CustomHandler:
```
CustomHandler(State, Value) -> {NewState, NewValue}
```

Событие:
```
frp_api:event({NetworkName, NodeName}, Msg).
```
Постоянные события (через каждый Interval (мс) на узел NodeName приходит событие Msg):
```
frp_api:timer({Network, NodeName}, Msg, Interval)
```

#### Реализация сети
Сеть реализована при помощи модуля [digraph]. Представляет собой ориентированный (возможно, цикличный) граф.

Вершины графа имеют уникальные идентификаторы и лейблы типа f_node. Ребра характеризуют зависимости одних значений от других.

Создание сети:
```
Network = frp_api:create_network()
```
Только что созданная сеть имеет узел ```external_entry``` с тождественной функцией и состоянием no_state.
Рекомендуется все узлы-входы (не имеющие значений, от которых зависят) определять как потомков этого узла

Добавление узлов в сеть:
```
frp_api:add_node(Network, {NodeName, Node})
```

```
Nodes = [{node_name1, Node1}, {node_name2, Node2}, ... {node_nameN, NodeN}]
frp_api:add_nodes(Network, Nodes)
```

Установление зависимостей между узлами:
```
frp_api:add_listener(Network, HostNode, Node)
```

```
Nodes = [{node_name1, Node1}, {node_name2, Node2}, ... {node_nameN, NodeN}]
frp_api:add_listeners(Network, HostNode, Nodes)
```

[digraph]:http://www.erlang.org/doc/man/digraph.html
