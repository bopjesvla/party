insert into game_players (game_slot_id, status, user_id, inserted_at, updated_at)
select empty_slot.id, 'playing', $2, $3, $3 from game_slots empty_slot
left join game_players p on (p.game_slot_id = empty_slot.id and p.status = 'playing')
where game_id = $1 and p.id is null
limit 1
