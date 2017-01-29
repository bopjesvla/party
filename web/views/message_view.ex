defmodule Mafia.MessageView do
  use Mafia.Web, :view

  def render("message.json", [{msg, u, {_, {h,m,_,_}}, type, ch}]) do
    %{msg: msg, u: u, ts: "#{h}:#{m}", ty: type, ch: ch}
  end
end
