with empty_slot as (
  select s.id from game_slots s
  left join game_players p on (p.game_slot_id = s.id and p.status = 'playing')
  where game_id = $1 and p.id is null
  limit 1
),
insert into game_players (game_slot_id, status, user_id)
values (empty_slot, 'playing', $2)