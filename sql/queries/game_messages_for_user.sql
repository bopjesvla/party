SELECT m.msg, m.user_id as u, m.inserted_at::text as ts, m.type as ty, m.channel_id as ch
FROM (
  SELECT m.*,
  between_agg(m.type = 'join' and m.user_id = $1, m.type = 'kick' and m.user_id = $1)
    over (partition by m.channel_id order by m.inserted_at) as between_joins_and_kicks
  FROM messages m
  JOIN channels c on (m.channel_id = c.id)
  WHERE c.game_id = $2 and (c.type = 'meet' or c.type = 'talk')
) m
WHERE between_joins_and_kicks;
