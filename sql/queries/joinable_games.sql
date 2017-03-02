select g.id, g.status as status, s.name as setup, s.size, count(empty.id) as empty from games g
join game_slots empty on sl.game_id = g.id
left join game_players p on (p.game_slot_id = empty_slot.id and p.status = 'playing')
where p.id is null and g.status = "ongoing"
order by g.inserted_at
group by g.id, s.name, s.size
limit 10
union all
select g.id, g.status as status, s.name as setup, s.size, count(empty.id) as empty from games g
join game_slots empty on sl.game_id = g.id
left join game_players p on (p.game_slot_id = empty_slot.id and p.status = 'playing')
where p.id is null and g.status = "signups"
order by g.inserted_at
group by g.id, s.name, s.size
limit 10
