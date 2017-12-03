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
| Connection_retry_timer_expired
| Hold_timer_expired
| Keepalive_timer_expired
| Tcp_CR_Acked
| Tcp_connection_confirmed
| Tcp_connection_fail
| BGP_open of Bgp.opent
| Bgp_header_err of Bgp.message_header_error
| Bgp_open_msg_err of Bgp.open_message_error
| Notif_msg_ver_err
| Notif_msg of Bgp.error
| Keepalive_msg
| Update_msg of Bgp.update
| Update_msg_err of Bgp.update_message_error

type action =
| Initiate_tcp_connection
| Listen_tcp_connection
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

let create conn_retry_time hold_time keepalive_time = {
  state = IDLE;
  conn_retry_counter = 0;
  conn_retry_time;
  hold_time;
  keepalive_time;
}

let handle_idle ({ state; conn_retry_counter; conn_retry_time; hold_time; keepalive_time } as fsm) = function
  | Manual_start -> 
    let actions = [Start_conn_retry_timer; Initiate_tcp_connection; Listen_tcp_connection] in
    let new_fsm = {
      state = CONNECT;
      conn_retry_counter = 0;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | _ -> (fsm, [])
;;

let handle_connect ({ state; conn_retry_counter; conn_retry_time; hold_time; keepalive_time } as fsm) = function
  | Manual_start -> (fsm, [])
  | Manual_stop ->
    let actions = [Drop_tcp_connection; Stop_conn_retry_timer] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = 0;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Connection_retry_timer_expired ->
    let actions = [Drop_tcp_connection; Start_conn_retry_timer; Initiate_tcp_connection; Listen_tcp_connection] in
    let new_fsm = {
      state = CONNECT;
      conn_retry_counter;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Tcp_CR_Acked | Tcp_connection_confirmed ->
    let actions = [Stop_conn_retry_timer; Send_open_msg; Start_hold_timer 240] in
    let new_fsm = {
      state = OPEN_SENT;
      conn_retry_counter;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Tcp_connection_fail ->
    let actions = [Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | _ ->
    let actions = [Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = conn_retry_counter + 1;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
;;

let handle_active ({ state; conn_retry_counter; conn_retry_time; hold_time; keepalive_time } as fsm) = function
  | Manual_start -> (fsm, [])
  | Manual_stop ->
    let actions = [Drop_tcp_connection; Stop_conn_retry_timer] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Connection_retry_timer_expired ->
    let actions = [Start_conn_retry_timer; Initiate_tcp_connection; Listen_tcp_connection] in
    let new_fsm = {
      state = CONNECT;
      conn_retry_counter;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Tcp_CR_Acked | Tcp_connection_confirmed ->
    let actions = [Send_open_msg; Start_hold_timer 240] in
    let new_fsm = {
      state = OPEN_SENT;
      conn_retry_counter;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Tcp_connection_fail ->
    let actions = [Reset_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = conn_retry_counter + 1;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | _ ->
    let actions = [Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = conn_retry_counter + 1;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
;;

let handle_open_sent ({ state; conn_retry_counter; conn_retry_time; hold_time; keepalive_time } as fsm) = function
  | Manual_start -> (fsm, [])
  | Manual_stop -> 
    let actions = [Send_msg (Bgp.Notification Bgp.Cease); Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = 0;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Hold_timer_expired ->
    let actions = [Send_msg (Bgp.Notification (Bgp.Hold_timer_expired)); Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = conn_retry_counter + 1;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Tcp_connection_fail ->
    let actions = [Drop_tcp_connection; Start_conn_retry_timer; Listen_tcp_connection] in
    let new_fsm = {
      state = ACTIVE;
      conn_retry_counter;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | BGP_open o ->
    let open Bgp in
    let remote_ht = o.hold_time in
    let nego_hold_time = if hold_time > remote_ht then remote_ht else hold_time in
    let actions = [Stop_conn_retry_timer; Send_msg Bgp.Keepalive; Start_keepalive_timer; Start_hold_timer nego_hold_time] in
    let new_fsm = {
      state = OPEN_CONFIRMED;
      conn_retry_counter;
      conn_retry_time;
      hold_time = nego_hold_time;
      keepalive_time = nego_hold_time / 3;
    } in
    (new_fsm, actions)
  | Bgp_header_err header_err ->
    let actions = [
      Send_msg (Bgp.Notification (Bgp.Message_header_error header_err)); 
      Drop_tcp_connection;
      Stop_conn_retry_timer;
    ] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = conn_retry_counter + 1;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Notif_msg_ver_err ->
    let actions = [Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | _ -> 
    let actions = [
      Send_msg (Bgp.Notification Bgp.Finite_state_machine_error);
      Stop_conn_retry_timer; 
      Drop_tcp_connection
    ] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = conn_retry_counter + 1;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
;;

let handle_open_confirmed  ({ state; conn_retry_counter; conn_retry_time; hold_time; keepalive_time } as fsm) = function
  | Manual_start -> (fsm, [])
  | Manual_stop -> 
    let actions = [
      Send_msg (Bgp.Notification Bgp.Cease); 
      Drop_tcp_connection; 
      Stop_conn_retry_timer
    ] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter=0;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Hold_timer_expired ->
    let actions = [
      Send_msg (Bgp.Notification (Bgp.Hold_timer_expired)); 
      Stop_conn_retry_timer;
      Drop_tcp_connection
    ] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = conn_retry_counter + 1;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Keepalive_timer_expired ->
    let actions = [Send_msg Bgp.Keepalive; Start_keepalive_timer] in
    (fsm, actions)
  | Tcp_connection_fail | Notif_msg _ ->
    let actions = [Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = conn_retry_counter + 1;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Notif_msg_ver_err ->
    let actions = [Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Keepalive_msg ->
    let actions = [Reset_hold_timer hold_time] in
    let new_fsm = {
      state = ESTABLISHED;
      conn_retry_counter;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | _ -> 
    let actions = [
      Send_msg (Bgp.Notification Bgp.Finite_state_machine_error);
      Stop_conn_retry_timer;
      Drop_tcp_connection;
    ] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = conn_retry_counter + 1;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
;;        

let handle_established ({ state; conn_retry_counter; conn_retry_time; hold_time; keepalive_time } as fsm) = function
  | Manual_start -> (fsm, [])
  | Manual_stop -> 
    let actions = [
      Send_msg (Bgp.Notification Bgp.Cease); 
      Drop_tcp_connection;
      Stop_conn_retry_timer;
    ] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = 0;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Hold_timer_expired ->
    let actions = [
      Send_msg (Bgp.Notification (Bgp.Hold_timer_expired)); 
      Stop_conn_retry_timer;
      Drop_tcp_connection
    ] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = conn_retry_counter + 1;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Keepalive_timer_expired ->
    let actions = [Send_msg Bgp.Keepalive; Start_keepalive_timer] in
    (fsm, actions)
  | Tcp_connection_fail | Notif_msg _ ->
    let actions = [Stop_conn_retry_timer; Drop_tcp_connection] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = conn_retry_counter + 1;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
    (new_fsm, actions)
  | Update_msg u ->
    (fsm, [Reset_hold_timer hold_time])
  | Keepalive_msg ->
    let actions = [Reset_hold_timer hold_time] in
    (fsm, actions)
  | _ ->
    let actions = [
      Send_msg (Bgp.Notification Bgp.Finite_state_machine_error);
      Stop_conn_retry_timer;
      Drop_tcp_connection;
    ] in
    let new_fsm = {
      state = IDLE;
      conn_retry_counter = conn_retry_counter + 1;
      conn_retry_time;
      hold_time;
      keepalive_time;
    } in
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
  








    


