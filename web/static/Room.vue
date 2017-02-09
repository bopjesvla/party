<template>
	<div class="room">
		<room-header :name="$route.params.name"></room-header>
		<div class="room-inner">
			<chat-messages :messages="messages"></chat-messages>
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
            if(d.active) {
              this.meets = d.active.map(meet => socket.channel("meet:" + meet.channel))
              console.log(this.meets)
              this.meets.forEach(x => x.join().receive("error", e => console.log(e)))
              this.activeChannel = this.meets[0]
            }
            else {
              this.meets = []
              this.activeChannel = this.channel
            }

	          this.meets.forEach(c => c.on("new:msg", msg => {
              msg.topic = c.topic
	            this.messages.push(msg)
	          }))
					})
					.receive("error", e => console.log(e))

				this.channel.on("new:msg", msg => {
					console.log(msg)
					this.messages.push(msg)
				})
			},
			send() {
				this.activeChannel.push("new:msg", {type: 'm', msg: this.input})
				this.input = ''
			}
		},
		watch: {
			$route: 'load'
		},
		components: {RoomHeader, ChatMessages, ChatInput},
		computed: {
			topic() {
				return `${this.$route.name}:${this.$route.params.name}`
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
