<template>
	<div class="room">
		<room-header :name="$route.params.game_id"></room-header>
		<div class="room-inner">
			<chat-messages :messages="messages" :players="this.info.players"></chat-messages>
			<div class="game-ui">
				<div class="active" v-if="info.active">
					<div class="channel" v-for="channel in info.active">
						{{channel}}
						<div class="role" v-if="channel.role">
							<b>{{channel.role.mods.join(" ")}} {{channel.role.role}}</b>
						</div>
						<div class="players" v-if="channel.members">
							<div class="player" v-for="slot in channel.members">
								{{slotName(slot)}}<br />
								votes
							</div>
						</div>
						<div class="act" v-if="channel.actions && channel.actions.length">
							<v-select
							  placeholder="Not Voting"
								:options="channel.actions"
								:custom-label="voteLabel"
								:value="isVoting(me.slot)"
								@input="vote($event)">
						</div>
					</div>
				</div>
			</div>
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
				input: "",
				meets: [],
				info: {}
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
						console.log(d)
						this.messages = d.msgs
						this.handleGameInfo(d)
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
			},
			handleGameInfo(d) {
				delete d.msgs
				this.info = d

				if(d.active) {
					this.meets = d.active.map(meet => socket.channel("meet:" + meet.channel))
					console.log(this.meets)
					this.activeChannel = this.meets[0]
				}
				else if(d.status != "ongoing") {
					this.meets = [socket.channel("talk:" + d.id)]
					this.activeChannel = this.meets[0]
				}

				this.meets.forEach(c => {
					c.join().receive("error", e => console.log(e))
					c.on("new:msg", msg => {
						msg.topic = c.topic
						this.messages.push(msg)
					})
				})
			},
			voteLabel(v) {
				let targets = v.opt.map(t => {
					if (typeof t == "number") {
						return this.slotName(t)
					}
					else {
						return t
					}
				})
				return `${v.act} ${targets.join(" ")}`
			},
			isVoting(slot) {
				// return this.votes.filter()
			},
			slotName(slot) {
				return this.info.players.filter(x => x.slot == slot)[0].name
			}
		},
		watch: {
			$route: 'load'
		},
		components: {RoomHeader, ChatMessages, ChatInput},
		computed: {
			topic() {
				return `${this.$route.name}:${this.$route.params.game_id}`
			},
			me() {
				return this.info.players.filter(x => x.user == window.user)[0].name
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
	.game-ui {
		position: absolute;
		top: 0;
		right: 0;
		width: 400px;
		max-width: 30%;
	}
</style>
