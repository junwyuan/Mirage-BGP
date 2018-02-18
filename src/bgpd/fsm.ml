type state =
  | IDLE
  | CONNECT
  | ACTIVE
  | OPEN_SENT
  | OPEN_CONFIRMED
  | ESTABLISHED

type t = {
  state: state;
  conn_retry_counter: int;
  conn_retry_time: int;
  hold_time: int;
  keepalive_time: int;
}


type event = 
  | Manual_start
  | Manual_stop
  | Automatic_start
  | Manual_start_passive_tcp
  | Automatic_start_passive_tcp
  | Automatic_stop
  | Connection_retry_timer_expired
  | Hold_timer_expired
  | Keepalive_timer_expired
  | Tcp_CR_Acked
  | Tcp_connection_confirmed
  | Tcp_connection_fail
  | BGP_open of Bgp.opent
  | Bgp_header_err of Bgp.message_header_error
  | Bgp_open_msg_err of Bgp.open_message_error
  | Open_collision_dump
  | Notif_msg_ver_err
  | Notif_msg of Bgp.error
  | Keepalive_msg
  | Update_msg of Bgp.update
  | Update_msg_err of Bgp.update_message_error

let event_to_string = function
  | Manual_start -> "Manual_start"
  | Manual_stop -> "Manual_stop"
  | Automatic_start -> "Automatic start"
  | Manual_start_passive_tcp -> "Manual start with passive tcp establishment"
  | Automatic_start_passive_tcp -> "Automatic start with passive tcp establishment"
  | Automatic_stop -> "Automatic stop"
  | Connection_retry_timer_expired -> "Conn retry timer expired."
  | Hold_timer_expired -> "Hold timer expired"
  | Keepalive_timer_expired -> "Keppalive timer expired"
  | Tcp_CR_Acked -> "TCP CR Acked"
  | Tcp_connection_confirmed -> "Tcp connection confirmed"
  | Tcp_connection_fail -> "Tcp connection failure"
  | BGP_open o -> Printf.sprintf "BGP OPEN: %s" (Bgp.opent_to_string o)
  | Bgp_header_err _ -> "BGP Header err"
  | Bgp_open_msg_err _ -> "Open Message err"
  | Open_collision_dump -> "Open collision dump"
  | Notif_msg_ver_err -> "Notif version error"
  | Notif_msg e -> Printf.sprintf "Notif msg: %s" (Bgp.to_string (Bgp.Notification e))
  | Keepalive_msg -> Printf.sprintf "Keepalive msg"
  | Update_msg u -> Printf.sprintf "Update msg: %s" (Bgp.update_to_string u)
  | Update_msg_err _ -> "Update msg err"


type action =
  | Initiate_tcp_connection
  | Drop_tcp_connection
  | Send_open_msg
  | Send_msg of Bgp.t
  | Start_conn_retry_timer
  | Stop_conn_retry_timer
  | Reset_conn_retry_timer
  | Start_hold_timer of int
  | Stop_hold_timer
  | Reset_hold_timer of int
  | Start_keepalive_timer
  | Stop_keepalive_timer
  | Reset_keepalive_timer
  | Process_update_msg of Bgp.update
  | Initiate_rib
  | Release_rib

let create conn_retry_time hold_time keepalive_time = {
  state = IDLE;
  conn_retry_counter = 0;
  conn_retry_time;
  hold_time;
  keepalive_time;
}

let set_state s t = {
  state = s;
  conn_retry_counter = t.conn_retry_counter;
  conn_retry_time = t.conn_retry_time;
  hold_time = t.hold_time;
  keepalive_time = t.keepalive_time;
}

let set_conn_retry_counter c t = {
  state = t.state;
  conn_retry_counter = c;
  conn_retry_time = t.conn_retry_time;
  hold_time = t.hold_time;
  keepalive_time = t.keepalive_time;
}

let set_conn_retry_time time t = {
  state = t.state;
  conn_retry_counter = t.conn_retry_counter;
  conn_retry_time = time;
  hold_time = t.hold_time;
  keepalive_time = t.keepalive_time;
}

let set_hold_time time t = {
  state = t.state;
  conn_retry_counter = t.conn_retry_counter;
  conn_retry_time = t.conn_retry_time;
  hold_time = time;
  keepalive_time = t.keepalive_time;
}

let set_keepalive_time time t = {
  state = t.state;
  conn_retry_counter = t.conn_retry_counter;
  conn_retry_time = t.conn_retry_time;
  hold_time = t.hold_time;
  keepalive_time = time;
}

let handle_idle fsm = function
  | Manual_start | Automatic_start -> 
    let actions = [Start_conn_retry_timer; Initiate_tcp_connection] in
    let new_fsm = fsm 
      |> set_state CONNECT 
      |> set_conn_retry_counter 0 
    in
    (new_fsm, actions)
  | Manual_stop | Automatic_stop -> (fsm, [])
  | Manual_start_passive_tcp | Automatic_start_passive_tcp ->
    let actions = [Start_conn_retry_timer] in
    let new_fsm = fsm
      |> set_state ACTIVE
      |> set_conn_retry_counter 0
    in
    (new_fsm, actions)
  | _ -> (fsm, [])
;;

let handle_connect fsm = function
  | Manual_start | Automatic_start
  | Manual_start_passive_tcp | Automatic_start_passive_tcp -> (fsm, [])
  | Manual_stop ->
    let actions = [Drop_tcp_connection; Stop_conn_retry_timer] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter 0 
    in
    (new_fsm, actions)
  | Connection_retry_timer_expired ->
    let actions = [Drop_tcp_connection; Start_conn_retry_timer; Initiate_tcp_connection] in
    let new_fsm = fsm |> set_state CONNECT in
    (new_fsm, actions)
  | Tcp_CR_Acked | Tcp_connection_confirmed ->
    let actions = [Stop_conn_retry_timer; Send_open_msg; Start_hold_timer 240] in
    let new_fsm = fsm |> set_state OPEN_SENT in
    (new_fsm, actions)
  | Tcp_connection_fail ->
    let actions = [Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = fsm |> set_state IDLE in
    (new_fsm, actions)
  | _ ->
    let actions = [Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
;;

let handle_active fsm = function
  | Manual_start | Automatic_start
  | Manual_start_passive_tcp | Automatic_start_passive_tcp -> (fsm, [])
  | Manual_stop ->
    let actions = [Drop_tcp_connection; Stop_conn_retry_timer] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter 0
    in
    (new_fsm, actions)
  | Connection_retry_timer_expired ->
    let actions = [Start_conn_retry_timer; Initiate_tcp_connection] in
    let new_fsm = fsm |> set_state CONNECT in
    (new_fsm, actions)
  | Tcp_CR_Acked | Tcp_connection_confirmed ->
    let actions = [
      Send_open_msg;
      Stop_conn_retry_timer;
      Start_hold_timer 240
    ] in
    let new_fsm = fsm |> set_state OPEN_SENT in
    (new_fsm, actions)
  | Tcp_connection_fail ->
    let actions = [Reset_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
  | _ ->
    let actions = [Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in    
    (new_fsm, actions)
;;

let handle_open_sent fsm = function
  | Manual_start | Automatic_start
  | Manual_start_passive_tcp | Automatic_start_passive_tcp -> (fsm, [])
  | Manual_stop -> 
    let actions = [Send_msg (Bgp.Notification Bgp.Cease); Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter 0 
    in
    (new_fsm, actions)
  | Hold_timer_expired ->
    let actions = [Send_msg (Bgp.Notification (Bgp.Hold_timer_expired)); Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
  | Tcp_connection_fail ->
    let actions = [Drop_tcp_connection; Start_conn_retry_timer] in
    let new_fsm = fsm |> set_state ACTIVE in
    (new_fsm, actions)
  | BGP_open o ->
    let open Bgp in
    let remote_ht = o.hold_time in
    let nego_hold_time = if fsm.hold_time > remote_ht then remote_ht else fsm.hold_time in
    let actions = [Stop_conn_retry_timer; Send_msg Bgp.Keepalive; Start_keepalive_timer; Reset_hold_timer nego_hold_time] in
    let new_fsm = fsm 
      |> set_state OPEN_CONFIRMED 
      |> set_hold_time nego_hold_time 
      |> set_keepalive_time (nego_hold_time / 3)
    in
    (new_fsm, actions)
  | Bgp_header_err header_err ->
    let actions = [
      Send_msg (Bgp.Notification (Bgp.Message_header_error header_err)); 
      Drop_tcp_connection;
      Stop_conn_retry_timer;
    ] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
  | Bgp_open_msg_err err ->
    let actions = [
      Send_msg (Bgp.Notification (Bgp.Open_message_error err)); 
      Drop_tcp_connection;
      Stop_conn_retry_timer;
    ] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
  | Open_collision_dump ->
    let actions = [
      Send_msg (Bgp.Notification Bgp.Cease);
      Stop_conn_retry_timer;
      Drop_tcp_connection;
    ] in
    let new_fsm = fsm
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1)
    in
    (new_fsm, actions)
  | Notif_msg_ver_err ->
    let actions = [Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = fsm |> set_state IDLE in
    (new_fsm, actions)
  | _ -> 
    let actions = [
      Send_msg (Bgp.Notification Bgp.Finite_state_machine_error);
      Stop_conn_retry_timer; 
      Drop_tcp_connection
    ] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
;;

let handle_open_confirmed fsm = function
  | Manual_start | Automatic_start
  | Manual_start_passive_tcp | Automatic_start_passive_tcp -> (fsm, [])
  | Manual_stop -> 
    let actions = [
      Send_msg (Bgp.Notification Bgp.Cease); 
      Drop_tcp_connection; 
      Stop_hold_timer;
      Stop_keepalive_timer;
      Stop_conn_retry_timer;
    ] in
    let new_fsm = fsm
      |> set_state IDLE 
      |> set_conn_retry_counter 0
    in
    (new_fsm, actions)
  | Hold_timer_expired ->
    let actions = [
      Send_msg (Bgp.Notification (Bgp.Hold_timer_expired)); 
      Stop_conn_retry_timer;
      Stop_hold_timer;
      Stop_keepalive_timer;
      Drop_tcp_connection
    ] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
  | Keepalive_timer_expired ->
    let actions = [Send_msg Bgp.Keepalive; Start_keepalive_timer] in
    (fsm, actions)
  | Tcp_connection_fail | Notif_msg _ ->
    let actions = [
      Stop_conn_retry_timer; 
      Stop_hold_timer;
      Stop_keepalive_timer;
      Drop_tcp_connection] 
    in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
  | Bgp_header_err err ->
    let actions = [
      Send_msg (Bgp.Notification (Bgp.Message_header_error err)); 
      Stop_conn_retry_timer;
      Stop_hold_timer;
      Stop_keepalive_timer;
      Drop_tcp_connection;
    ] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
  | Bgp_open_msg_err err ->
    let actions = [
      Send_msg (Bgp.Notification (Bgp.Open_message_error err)); 
      Stop_conn_retry_timer;
      Stop_hold_timer;
      Stop_keepalive_timer;
      Drop_tcp_connection;
    ] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
  | Notif_msg_ver_err ->
    let actions = [
      Stop_conn_retry_timer; 
      Stop_hold_timer;
      Stop_keepalive_timer;
      Drop_tcp_connection
    ] in
    let new_fsm = fsm |> set_state IDLE in
    (new_fsm, actions)
  | Open_collision_dump ->
    let actions = [
      Send_msg (Bgp.Notification Bgp.Cease);
      Stop_conn_retry_timer;
      Stop_hold_timer;
      Stop_keepalive_timer;
      Drop_tcp_connection;
    ] in
    let new_fsm = fsm
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1)
    in
    (new_fsm, actions)
  | Keepalive_msg ->
    let actions = [Send_msg Bgp.Keepalive; Reset_hold_timer fsm.hold_time; Initiate_rib] in
    let new_fsm = fsm |> set_state ESTABLISHED in
    (new_fsm, actions)
  | _ -> 
    let actions = [
      Send_msg (Bgp.Notification Bgp.Finite_state_machine_error);
      Stop_conn_retry_timer;
      Stop_hold_timer;
      Stop_keepalive_timer;
      Drop_tcp_connection;
    ] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
;;        

let handle_established fsm = function
  | Manual_start | Automatic_start
  | Manual_start_passive_tcp | Automatic_start_passive_tcp -> (fsm, [])
  | Manual_stop -> 
    let actions = [
      Send_msg (Bgp.Notification Bgp.Cease); 
      Release_rib;
      Drop_tcp_connection;
      Stop_conn_retry_timer;
      Stop_hold_timer;
      Stop_keepalive_timer;
    ] in
    let new_fsm = fsm
      |> set_state IDLE
      |> set_conn_retry_counter 0
    in
    (new_fsm, actions)
  | Hold_timer_expired ->
    let actions = [
      Send_msg (Bgp.Notification (Bgp.Hold_timer_expired)); 
      Release_rib;
      Stop_conn_retry_timer;
      Stop_hold_timer;
      Stop_keepalive_timer;
      Drop_tcp_connection;
    ] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
  | Keepalive_timer_expired ->
    let actions = [Send_msg Bgp.Keepalive; Reset_keepalive_timer] in
    (fsm, actions)
  | Tcp_connection_fail | Notif_msg _ ->
    let actions = [
      Stop_conn_retry_timer; 
      Stop_hold_timer;
      Stop_keepalive_timer;
      Release_rib; 
      Drop_tcp_connection
    ] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
  | Update_msg u ->
    (fsm, [Reset_hold_timer fsm.hold_time; Process_update_msg u])
  | Bgp_header_err err ->
    let actions = [
      Send_msg (Bgp.Notification (Bgp.Message_header_error err)); 
      Stop_conn_retry_timer; 
      Stop_hold_timer;
      Stop_keepalive_timer;
      Release_rib;
      Drop_tcp_connection;
    ] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
  | Update_msg_err err ->
    let actions = [
      Send_msg (Bgp.Notification (Bgp.Update_message_error err)); 
      Stop_conn_retry_timer; 
      Stop_hold_timer;
      Stop_keepalive_timer;
      Release_rib;
      Drop_tcp_connection;
    ] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
  | Keepalive_msg ->
    let actions = [Reset_hold_timer fsm.hold_time] in
    (fsm, actions)
  | _ ->
    let actions = [
      Send_msg (Bgp.Notification Bgp.Finite_state_machine_error);
      Stop_conn_retry_timer;
      Stop_hold_timer;
      Stop_keepalive_timer;
      Drop_tcp_connection;
    ] in
    let new_fsm = fsm 
      |> set_state IDLE 
      |> set_conn_retry_counter (fsm.conn_retry_counter + 1) 
    in
    (new_fsm, actions)
;;

let handle t event =
  match t.state with
  | IDLE -> handle_idle t event
  | CONNECT -> handle_connect t event
  | ACTIVE -> handle_active t event
  | OPEN_SENT -> handle_open_sent t event
  | OPEN_CONFIRMED -> handle_open_confirmed t event
  | ESTABLISHED -> handle_established t event
;;

let to_string t = 
  let state_msg = 
    match t.state with
    | IDLE -> "IDLE"
    | CONNECT -> "CONNECT"
    | ACTIVE -> "ACTIVE"
    | OPEN_SENT -> "OPEN_SENT"
    | OPEN_CONFIRMED -> "OPEN_CONFIRMED"
    | ESTABLISHED -> "ESTABLISHED"
  in
  Printf.sprintf "{state: %s; conn_retry_counter: %d; conn_retry_time: %d; hold_time: %d; keepalive_time: %d;}"
                state_msg t.conn_retry_counter t.conn_retry_time t.hold_time t.keepalive_time
;;
  
let state t = t.state

