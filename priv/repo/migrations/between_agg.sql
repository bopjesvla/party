-- between_fn is true when the starting condition ($2) is true, false when the ending condition ($3) is true, previous state otherwise
    
CREATE FUNCTION between_fn(prev_state boolean, start_cond boolean, end_cond boolean) RETURNS boolean AS $$
select case when $2 then true when $3 then false else $1 end;
$$ LANGUAGE SQL;

-----

CREATE AGGREGATE between_agg(boolean, boolean)
(
  sfunc = between_fn,
  stype = boolean,
  initcond = false
);

-----

CREATE FUNCTION messages_between_joins_and_kicks(user_id integer, game text)
RETURNS table(msg text, u text, ts text, ty text, ch int) AS $$
SELECT m.msg, u.name as m, m.inserted_at::text as ts, m.type as ty, m.channel_id as ch
FROM (
  SELECT m.*,
  between_agg(m.type = 'join' and m.user_id = $1, m.type = 'kick' and m.user_id = $1) over (partition by m.channel_id order by m.inserted_at) as between_joins_and_kicks
  FROM messages m
  JOIN channels c on (m.channel_id = c.id)
  JOIN games g on (c.game_id = g.id)
  WHERE g.name = $2 and c.type = 'm'
) m
JOIN users u on(m.user_id = u.id)
WHERE between_joins_and_kicks;
$$ LANGUAGE SQL;
