<template>
	<div class="messages">
		<div :class="`message type-${message.ty}`" v-for="message in messages">
			<div class="message-ts">
				{{time(message.ts)}}
			</div>
			<div class="message-user">
				{{name(message.u)}}
			</div>
			<div class="msg" v-if="message.ty == 'flip'">
				flipped
				<team :team="t" v-for="t in JSON.parse(message.msg).teams"></team>
				<role :role="r" v-for="r in JSON.parse(message.msg).roles"></role>
			</div>
			<div class="msg" v-else>
				{{renderMessage(message)}}
			</div>
		</div>
	</div>
</template>

<script>
	import {renderVote, renderEnd} from '../textviews'
	import Role from './Role.vue'
	import Team from './Team.vue'

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
				if (message.ty == "end") {
					return renderEnd(JSON.parse(message.msg).winners, this.players)
				}

				return message.msg || message.ty
			}
		},
		components: {Role, Team}
	}
</script>

<style>
	.messages {
		display: table;
		border-spacing: 10px 0;
		padding-left: 2%;
		padding-right: 2%;
		width: 70%;
		.message {
			display: table-row;
			color: grey;
			.msg {
				width: 100%;
			}
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
			&.type-join, &.type-kick {
				display: none;
			}
			&.type-m {
				.msg {
					color: black;
					word-break: break-word;
				}
			}
			&.type-vote {
				.msg {
					color: black;
					font-style: italic;
				}
			}
			&.type-phase, &.type-end {
				.msg {
					padding-top: 19px;
					color: black;
					font-size: 2em;
				}
			}
			&.type-phase {
				.msg {
					text-transform: capitalize;
				}
			}
			&.type-sys {
				.msg {
					color: red;
				}
			}
			&.type-flip {
				.msg {
					color: purple;
				}
			}
		}
		.message:last-child .msg {
			padding-bottom: 5px;
		}
	}
</style>
