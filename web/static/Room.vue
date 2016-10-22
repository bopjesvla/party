<template>
	<div class="room">
		<room-header :name=$route.params.name></room-header>
		<room-messages :messages=messages></room-messages>
		<chat-input :channel=channel></chat-input>
	</div>
</template>
<script>
	import socket from './socket'

	export default {
		data() {
			return {
				messages: null,
				channel: socket.channel(`room:${this.$route.params.name}`)
			}
		},
		created() {
			this.channel.join()
				.receive("ok", ({chans, msgs}) => {
					this.messages = msgs
					this.channels = chan
				})
				.receive("error", e => console.log(e))
		}
	}
</script>
