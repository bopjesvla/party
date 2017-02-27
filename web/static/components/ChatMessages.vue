<template>
	<div class="messages">
		<div :class="`message type-${message.ty}`" v-for="message in messages">
			<div class="message-ts">
				{{time(message.ts)}}
			</div>
			<div class="message-user">
				{{name(message.u)}}
			</div>
			<div class="msg">
				{{renderMessage(message)}}
			</div>
		</div>
	</div>
</template>

<script>
	import {renderVote} from '../textviews'
	export default {
		props: ['messages', 'players'],
		methods: {
			time: x => x.split(/[T ]/)[1].slice(0,5),
			name(u) {
				return typeof u == "number" ? this.players.filter(x => x.user == u)[0].name : u
			},
			renderMessage(message) {
				if (message.ty == "m") {
					return message.msg
				}
				if (message.ty == "vote") {
					return "votes to " + renderVote(JSON.parse(message.msg), this.players)
				}

				return message.msg || message.ty
			}
		}
	}
</script>

<style>
	.messages {
		display: table;
		border-spacing: 10px 0;
		padding-left: 2%;
		.message {
			display: table-row;
			color: grey;
			.message-ts, .message-user, .msg {
				display: table-cell;
				padding-top: 2px;
			}
			.message-ts {
				font-size: .8em;
				padding-right: 10px;
			}
			.message-user {
				padding-right: 5px;
				border-right: 1px silver solid;
				color: brown;
			}
			&.type-m {
				.msg {
					color: black;
				}
			}
			&.type-vote {
				.msg {
					color: black;
					font-style: italic;
				}
			}
			&.type-phase {
				.msg {
					color: black;
					font-size: 3em;
					text-transform: capitalize;
				}
			}
		}
	}
</style>
