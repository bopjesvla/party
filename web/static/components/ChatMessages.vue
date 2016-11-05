<template>
	<div class="messages">
		<div class="message" v-for="message in messages">
			<div class="message-ts">
				{{message.ts | time}}
			</div>
			<div class="message-user">
				{{message.u}}
			</div>
			<div class="msg">
				{{message.msg}}
			</div>
		</div>
	</div>
</template>

<script>
	export default {
		props: ['messages'],
		filters: {
			time: x => x.split('T')[1].slice(0,5)
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
			.message-ts, .message-user, .msg {
				display: table-cell;
				padding-top: 2px;
			}
			.message-ts {
				color: grey;
				font-size: .8em;
				padding-right: 10px;
			}
			.message-user {
				padding-right: 5px;
				border-right: 1px silver solid;
				color: brown;
			}
		}
	}
</style>
