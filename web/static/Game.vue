<template>
	<div class="room">
		<room-header :name="$route.params.game_id"></room-header>
		<div class="room-inner">
			<chat-messages :messages="messages" :players="this.info.players"></chat-messages>
			<div class="game-ui">
				<div class="active" v-if="info.active">
					<div class="channel" v-for="channel in roleChannels(info.active)">
						<div class="role" v-if="channel.role">
							<b>{{channel.role.mods.join(" ")}} {{channel.role.role}}</b>
						</div>
						<div class="players" v-if="channel.members">
							<div class="player" v-for="slot in channel.members">
								{{slotName(slot, info.players)}}<br />
								<div class="vote" v-if="isVoting(channel, slot)">
									votes to {{
										renderVote(isVoting(channel, slot), info.players)
									}}
								</div>
								<div class="vote">
									voted by {{
										renderVotedBy(channel, slot) || "no one"
									}}
								</div>
							</div>
						</div>
						<div class="act" v-if="channel.actions && channel.actions.length">
							<v-select
							  placeholder="Vote"
								:options="voteOptions(channel)"
								track-by="vote"
								label="label"
								:value="isVoting(channel, me.slot)"
								@input="vote(channel, $event)">
							</v-select>
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
	import {renderVote, slotName} from './textviews'
	import Vote from './components/Vote'

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
			renderVote,
			slotName,
			voteOptions(channel) {
				return channel.actions.map(x => ({
					vote: x,
					label: renderVote(x, this.info.players)
				}))
			},
			isVoting(channel, slot) {
				return channel.votes.filter(x => x.player == slot)[0]
			},
			renderVotedBy(channel, slot) {
				return channel.votes
				  .filter(x => ~x.opt.indexOf(slot))
					.map(x => slotName(x.player, this.info.players))
					.join(", ")
			},
			actor(vote) {
				return vote.player
			},
			roleChannels(channels) {
				return channels.filter(c => c.type != "player")
			},
			vote(channel, v) {
				this.meets.filter(x => x.topic == "meet:" + channel.channel)[0]
					.push("new:vote", v.vote)
			}
		},
		watch: {
			$route: 'load'
		},
		components: {RoomHeader, ChatMessages, ChatInput, Vote},
		computed: {
			topic() {
				return `${this.$route.name}:${this.$route.params.game_id}`
			},
			me() {
				return this.info.players.filter(x => x.user == window.user)[0]
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
