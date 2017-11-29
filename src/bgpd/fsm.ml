module type S = sig
    type flow

    type state =
    | IDLE
    | CONNECT
    | ACTIVE
    | OPEN_SENT
    | OPEN_CONFIRMED
    | ESTABLISHED

    type t = {
        state: state;
        connect_retry_timer: float;
        connect_retry_counter: int;
        connect_retry_time: float;
        hold_timer: float;
        hold_time: float;
        keepalive_timer: float;
        keepalive_time: float;
    }

    type event = 
    | Manual_start
    | Manual_stop
    | Connection_retry_timer_expired
    | Hold_timer_expired
    | Keepalive_timer_expired
    | Tcp_CR_Acked of flow
    | Tcp_connection_confirmed of flow
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
    | Drop_tcp_connection
    | Send_open_msg
    | Send_msg of Bgp.t
    
    val create : float -> float -> float -> t
    val handle : t -> event -> t * action list
end

module Make (Stack: Mirage_stack_lwt.V4) : (S with type flow = Stack.TCPV4.flow) = struct
    type flow = Stack.TCPV4.flow

    type state =
    | IDLE
    | CONNECT
    | ACTIVE
    | OPEN_SENT
    | OPEN_CONFIRMED
    | ESTABLISHED

    type t = {
        state: state;
        connect_retry_timer: float;
        connect_retry_counter: int;
        connect_retry_time: float;
        hold_timer: float;
        hold_time: float;
        keepalive_timer: float;
        keepalive_time: float;
    }

    type event = 
    | Manual_start
    | Manual_stop
    | Connection_retry_timer_expired
    | Hold_timer_expired
    | Keepalive_timer_expired
    | Tcp_CR_Acked of flow
    | Tcp_connection_confirmed of flow
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
    | Drop_tcp_connection
    | Send_open_msg
    | Send_msg of Bgp.t


    let create connect_retry_time hold_time keepalive_time = {
        state=IDLE;
        connect_retry_time;
        connect_retry_timer = connect_retry_time;
        connect_retry_counter = 0;
        hold_timer = hold_time;
        hold_time;
        keepalive_timer = keepalive_time;
        keepalive_time;
    };;

    let handle_idle ({state; connect_retry_counter; connect_retry_time; 
                    connect_retry_timer; hold_timer; hold_time; keepalive_time; 
                    keepalive_timer} as fsm) = function
        | Manual_start -> 
            let actions = [Initiate_tcp_connection] in
            let new_fsm = {
                state=CONNECT;
                connect_retry_counter=0;
                connect_retry_timer=connect_retry_time;
                connect_retry_time;
                hold_timer;
                hold_time;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | _ -> (fsm, [])
    ;;

    let handle_connect ({state; connect_retry_counter; connect_retry_time; 
                    connect_retry_timer; hold_timer; hold_time; keepalive_time; 
                    keepalive_timer} as fsm) = function
        | Manual_stop ->
            let actions = [Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_counter=0;
                connect_retry_timer=0.;
                connect_retry_time;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Connection_retry_timer_expired ->
            let actions = [Initiate_tcp_connection] in
            let new_fsm = {
                state=CONNECT;
                connect_retry_timer=connect_retry_time;
                connect_retry_time;
                connect_retry_counter;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Tcp_CR_Acked _ | Tcp_connection_confirmed _ ->
            let actions = [Send_open_msg] in
            let new_fsm = {
                state=OPEN_SENT;
                connect_retry_counter;
                connect_retry_timer=0.;
                connect_retry_time;
                hold_time;
                hold_timer=240.; (* 4 minutes *)
                keepalive_timer;
                keepalive_time;
            } in
            (new_fsm, actions)
        | Tcp_connection_fail ->
            let actions = [Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_counter;
                connect_retry_time;
                connect_retry_timer;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Manual_start -> (fsm, [])
        | _ ->
            let actions = [Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_counter=connect_retry_counter + 1;
                connect_retry_time;
                connect_retry_timer=0.;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
    ;;

    let handle_active ({state; connect_retry_counter; connect_retry_time; 
                    connect_retry_timer; hold_timer; hold_time; keepalive_time; 
                    keepalive_timer} as fsm) = function
        | Manual_start -> (fsm, [])
        | Manual_stop ->
            let actions = [Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_counter;
                connect_retry_time;
                connect_retry_timer;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Connection_retry_timer_expired ->
            let actions = [Initiate_tcp_connection] in
            let new_fsm = {
                state=CONNECT;
                connect_retry_counter;
                connect_retry_time;
                connect_retry_timer=connect_retry_time;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Tcp_CR_Acked _ | Tcp_connection_confirmed _ ->
            let actions = [Send_open_msg] in
            let new_fsm = {
                state=OPEN_SENT;
                connect_retry_counter;
                connect_retry_timer=0.;
                connect_retry_time;
                hold_time;
                hold_timer=240.; (* 4 minutes *)
                keepalive_timer;
                keepalive_time;
            } in
            (new_fsm, actions)
        | Tcp_connection_fail ->
            let actions = [Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_counter=connect_retry_counter + 1;
                connect_retry_time;
                connect_retry_timer;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | _ ->
            let actions = [Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_counter=connect_retry_counter + 1;
                connect_retry_time;
                connect_retry_timer=0.;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
    ;;

    let handle_open_sent ({state; connect_retry_counter; connect_retry_time; 
                    connect_retry_timer; hold_timer; hold_time; keepalive_time; 
                    keepalive_timer} as fsm) = function
        | Manual_start -> (fsm, [])
        | Manual_stop -> 
            let actions = [Send_msg (Bgp.Notification Bgp.Cease) ; Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_timer=0.;
                connect_retry_counter=0;
                connect_retry_time;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Hold_timer_expired ->
            let actions = [Send_msg (Bgp.Notification (Bgp.Hold_timer_expired)); Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_counter=connect_retry_counter + 1;
                connect_retry_time;
                connect_retry_timer=0.;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Tcp_connection_fail ->
            let actions = [] in
            let new_fsm = {
                state=ACTIVE;
                connect_retry_counter;
                connect_retry_time;
                connect_retry_timer=connect_retry_time;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | BGP_open o ->
            let open Bgp in
            let actions = [Send_msg Bgp.Keepalive] in
            let remote_ht = float_of_int o.hold_time in
            let nego_hold_time = if hold_time > remote_ht then remote_ht else hold_time in
            let new_fsm = {
                state = OPEN_CONFIRMED;
                connect_retry_counter;
                connect_retry_time;
                connect_retry_timer = 0.;
                hold_time = nego_hold_time;
                hold_timer = nego_hold_time;
                keepalive_time = nego_hold_time /. 3.;
                keepalive_timer = nego_hold_time /. 3.;
            } in
            (new_fsm, actions)
        | Bgp_header_err header_err ->
            let actions = [Send_msg (Bgp.Notification (Bgp.Message_header_error header_err)); Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_counter = connect_retry_counter + 1;
                connect_retry_time;
                connect_retry_timer=0.;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Notif_msg_ver_err ->
            let actions = [Drop_tcp_connection] in
            let new_fsm = {
                state = IDLE;
                connect_retry_counter;
                connect_retry_time;
                connect_retry_timer;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | _ -> (fsm, [])
    ;;

    let handle_open_confirmed ({state; connect_retry_counter; connect_retry_time; 
                    connect_retry_timer; hold_timer; hold_time; keepalive_time; 
                    keepalive_timer} as fsm) = function
        | Manual_start -> (fsm, [])
        | Manual_stop -> 
            let actions = [Send_msg (Bgp.Notification Bgp.Cease) ; Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_timer=0.;
                connect_retry_counter=0;
                connect_retry_time;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Hold_timer_expired ->
            let actions = [Send_msg (Bgp.Notification (Bgp.Hold_timer_expired)); Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_counter=connect_retry_counter + 1;
                connect_retry_time;
                connect_retry_timer=0.;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Keepalive_timer_expired ->
            let actions = [Send_msg Bgp.Keepalive] in
            let new_fsm = {
                state;
                connect_retry_counter;
                connect_retry_time;
                connect_retry_timer;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer = keepalive_time;
            } in
            (new_fsm, actions)
        | Tcp_connection_fail | Notif_msg _ ->
            let actions = [Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_counter = connect_retry_counter + 1;
                connect_retry_time;
                connect_retry_timer = 0.;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Notif_msg_ver_err ->
            let actions = [Drop_tcp_connection] in
            let new_fsm = {
                state = IDLE;
                connect_retry_counter;
                connect_retry_time;
                connect_retry_timer;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Keepalive_msg ->
            let new_fsm = {
                state = ESTABLISHED;
                connect_retry_counter;
                connect_retry_time;
                connect_retry_timer;
                hold_time;
                hold_timer = hold_time;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, [])
        | _ -> (fsm, [Drop_tcp_connection])
    ;;        

    let handle_established ({state; connect_retry_counter; connect_retry_time; 
                    connect_retry_timer; hold_timer; hold_time; keepalive_time; 
                    keepalive_timer} as fsm) = function
        | Manual_start -> (fsm, [])
        | Manual_stop -> 
            let actions = [Send_msg (Bgp.Notification Bgp.Cease) ; Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_timer=0.;
                connect_retry_counter=0;
                connect_retry_time;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Hold_timer_expired ->
            let actions = [Send_msg (Bgp.Notification (Bgp.Hold_timer_expired)); Drop_tcp_connection] in
            let new_fsm = {
                state=IDLE;
                connect_retry_counter=connect_retry_counter + 1;
                connect_retry_time;
                connect_retry_timer=0.;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Keepalive_timer_expired ->
            let actions = [Send_msg Bgp.Keepalive] in
            let new_fsm = {
                state;
                connect_retry_counter;
                connect_retry_time;
                hold_timer;
                connect_retry_timer;
                hold_time;
                keepalive_time;
                keepalive_timer = keepalive_time;
            } in
            (new_fsm, actions)
        | Notif_msg err ->
            let actions = [Drop_tcp_connection] in
            let new_fsm = {
                state = IDLE;
                connect_retry_counter = connect_retry_counter + 1;
                connect_retry_time;
                connect_retry_timer = 0.;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, actions)
        | Keepalive_msg ->
            let new_fsm = {
                state;
                connect_retry_counter;
                connect_retry_time;
                connect_retry_timer;
                hold_time;
                hold_timer = hold_time;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, [])
        | _ ->
            let new_fsm = {
                state = IDLE;
                connect_retry_counter = connect_retry_counter + 1;
                connect_retry_time;
                connect_retry_timer = 0.;
                hold_time;
                hold_timer;
                keepalive_time;
                keepalive_timer;
            } in
            (new_fsm, [])
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
end






    


