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
  | Notif_msg of Bgp.msg_fmt_error
  | Keepalive_msg
  | Update_msg of Bgp.update
  | Update_msg_err of Bgp.update_message_error

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

val event_to_string : event -> string
val create : int -> int -> int -> t
val handle : t -> event -> t * action list
val to_string : t -> string
val state : t -> state