<template>
	<form class="chatInput" action="">
		<div class="username">{{username}}</div>
		<input class="chat" type="text" v-model="msg">
		<div class="channels" v-if="channels && channels.length > 1">
			<select options="channels">
				<option value="channel" v-for="channel in channels">{{channel}}</option>
			</select>
		</div>
	</form>
</template>
<script>
	export default {
		data() {
			return {
				channels: [],
				msg: ""
			}
		}
	}
</script>
