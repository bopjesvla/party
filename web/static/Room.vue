<template>
	<div class="room">
		<room-header :name=$route.params.name></room-header>
		<div class="room-inner">
			<chat-messages :messages=messages></chat-messages>
		</div>
		<form @submit.prevent="send">
			<input type="text" v-model="input"/>
		</form>
		<!--<chat-input :channel={[$route.params.name]:topic}></chat-input>-->
	</div>
</template>
<script>
	import socket from './socket'
	import RoomHeader from './components/RoomHeader'
	import ChatMessages from './components/ChatMessages'
	import ChatInput from './components/ChatInput'

	export default {
		data() {
			return {
				messages: null,
				input: ""
			}
		},
		created() {
			this.load()
		},
		methods: {
			load() {
				this.channel = socket.channel(this.topic)
				this.channel.join()
					.receive("ok", (d) => {
						console.log(d.msgs)
						this.messages = d.msgs
					})
					.receive("error", e => console.log(e))

				this.channel.on("new_msg", msg => {
					this.messages.push(msg)
				})
			},
			send() {
				this.channel.push("new_msg", {type: 'm', msg: this.input})
				this.input = ''
			}
		},
		watch: {
			$route: 'load'
		},
		components: {RoomHeader, ChatMessages, ChatInput},
		computed: {
			topic() {
				return `room:${this.$route.params.name}`
			},
			channels() {
				return [{
				    [this.$route.params.name]: this.topic
				}]
			}
		}
	}
</script>
<style>
	.room {
		> .room-inner {
			overflow: auto;
			position: absolute;
			width: 100%;
			top: 3.8em;
			border-top: 1px solid silver;
			bottom: 2em;
			margin-bottom: 10px;
		}
		> form {
			position: absolute;
			bottom: 0;
			left: 0;
			right: 0;
			display: flex;
			background-color: silver;
			height: 2.4em;
			> input {
				border: 1px solid grey;
				background-color: #fff;
				margin: .4em;
				height: 1.6em;
				padding-left: 5px;
				width: 100%;
			}
		}
	}
</style>
