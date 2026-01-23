import QtQuick
import QtQuick.Layouts
import QtQuick.Controls.Fusion
import Quickshell.Wayland

Rectangle {
	id: root
	required property LockContext context
	
	// Custom theme colors matching your system
	readonly property color bgColor: "#222222"
	readonly property color fgColor: "#ffffff"
	readonly property color accentColor: "#285577"
	readonly property color borderColor: "#333333"
	readonly property color errorColor: "#da4453"
	readonly property color inputBgColor: "#2b2b2b"
	
	color: bgColor

	// Wallpaper background with blur effect
	Image {
		id: wallpaper
		anchors.fill: parent
		source: "file:///home/xynapz/.cache/current_wallpaper.jpg"
		fillMode: Image.PreserveAspectCrop
		
		// Fallback if wallpaper doesn't exist
		onStatusChanged: {
			if (status === Image.Error) {
				visible = false;
			}
		}
		
		// Dark overlay for readability
		Rectangle {
			anchors.fill: parent
			color: "#000000"
			opacity: 0.6
		}
		
		// Subtle gradient overlay
		Rectangle {
			anchors.fill: parent
			gradient: Gradient {
				GradientStop { position: 0.0; color: "#00000000" }
				GradientStop { position: 0.5; color: "#00000000" }
				GradientStop { position: 1.0; color: "#40000000" }
			}
		}
	}

	// User info at top - left side
	RowLayout {
		id: userInfo
		anchors {
			top: parent.top
			left: parent.left
			margins: 30
		}
		spacing: 15

		// User icon
		Rectangle {
			width: 50
			height: 50
			radius: 25
			color: root.accentColor
			border.color: root.fgColor
			border.width: 2

			Label {
				anchors.centerIn: parent
				text: "👤"
				font.pointSize: 24
			}
		}

		ColumnLayout {
			spacing: 2

			Label {
				text: Qt.platform.os === "linux" ? (process.env.USER || "User") : "User"
				font.family: "sans-serif"
				font.pointSize: 14
				font.weight: Font.Bold
				color: root.fgColor
			}

			Label {
				text: "Locked"
				font.family: "sans-serif"
				font.pointSize: 10
				color: "#bbbbbb"
			}
		}
	}

	// System status at top right
	RowLayout {
		id: systemStatus
		anchors {
			top: parent.top
			right: parent.right
			margins: 30
		}
		spacing: 20

		// Battery indicator
		RowLayout {
			spacing: 5
			
			Label {
				text: "🔋"
				font.pointSize: 16
			}
			
			Label {
				text: "Battery"
				font.family: "sans-serif"
				font.pointSize: 11
				color: "#bbbbbb"
			}
		}

		// WiFi indicator
		RowLayout {
			spacing: 5
			
			Label {
				text: "📶"
				font.pointSize: 16
			}
			
			Label {
				text: "WiFi"
				font.family: "sans-serif"
				font.pointSize: 11
				color: "#bbbbbb"
			}
		}
	}


	// Main content area
	ColumnLayout {
		anchors.centerIn: parent
		spacing: 40

		// Clock display with enhanced styling
		ColumnLayout {
			Layout.alignment: Qt.AlignHCenter
			spacing: 5

			Label {
				id: clock
				property var date: new Date()

				Layout.alignment: Qt.AlignHCenter

				renderType: Text.NativeRendering
				font.family: "sans-serif"
				font.pointSize: 100
				font.weight: Font.Bold
				color: root.fgColor
				
				// Subtle shadow effect
				style: Text.Outline
				styleColor: "#40000000"

				Timer {
					running: true
					repeat: true
					interval: 1000
					onTriggered: clock.date = new Date();
				}

				text: {
					const hours = this.date.getHours().toString().padStart(2, '0');
					const minutes = this.date.getMinutes().toString().padStart(2, '0');
					const seconds = this.date.getSeconds().toString().padStart(2, '0');
					return `${hours}:${minutes}:${seconds}`;
				}
			}


			// Date display
			Label {
				id: dateLabel
				property var date: clock.date

				Layout.alignment: Qt.AlignHCenter
				Layout.topMargin: 10

				font.family: "sans-serif"
				font.pointSize: 18
				font.weight: Font.Medium
				color: "#dddddd"

				text: {
					const days = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];
					const months = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
					const dayName = days[this.date.getDay()];
					const monthName = months[this.date.getMonth()];
					const day = this.date.getDate();
					const year = this.date.getFullYear();
					return `${dayName}, ${monthName} ${day}, ${year}`;
				}
			}
		}

		// Password entry area with card-like design
		Rectangle {
			Layout.alignment: Qt.AlignHCenter
			Layout.topMargin: 20
			width: 500
			height: passwordColumn.height + 40
			color: "#1a1a1a"
			opacity: 0.9
			border.color: root.borderColor
			border.width: 1
			radius: 8

			ColumnLayout {
				id: passwordColumn
				anchors.centerIn: parent
				width: parent.width - 40
				spacing: 15

				Label {
					Layout.alignment: Qt.AlignHCenter
					text: "Enter Password to Unlock"
					font.family: "sans-serif"
					font.pointSize: 12
					color: "#bbbbbb"
				}

				RowLayout {
					Layout.alignment: Qt.AlignHCenter
					spacing: 10

					TextField {
						id: passwordBox

						implicitWidth: 350
						implicitHeight: 50
						padding: 15

						focus: true
						enabled: !root.context.unlockInProgress
						echoMode: TextInput.Password
						inputMethodHints: Qt.ImhSensitiveData
						placeholderText: "Password"

						font.family: "sans-serif"
						font.pointSize: 12

						background: Rectangle {
							color: root.inputBgColor
							border.color: passwordBox.activeFocus ? root.accentColor : root.borderColor
							border.width: 2
							radius: 4
						}

						color: root.fgColor

						onTextChanged: root.context.currentText = this.text;
						onAccepted: root.context.tryUnlock();

						Connections {
							target: root.context
							function onCurrentTextChanged() {
								passwordBox.text = root.context.currentText;
							}
						}
					}

					Button {
						id: unlockButton
						text: "→"
						implicitWidth: 50
						implicitHeight: 50

						focusPolicy: Qt.NoFocus
						enabled: !root.context.unlockInProgress && root.context.currentText !== "";

						font.family: "sans-serif"
						font.pointSize: 20
						font.weight: Font.Bold

						background: Rectangle {
							color: unlockButton.enabled ? root.accentColor : "#555555"
							border.color: unlockButton.enabled ? root.accentColor : root.borderColor
							border.width: 1
							radius: 4
						}

						contentItem: Text {
							text: unlockButton.text
							font: unlockButton.font
							color: unlockButton.enabled ? root.fgColor : "#888888"
							horizontalAlignment: Text.AlignHCenter
							verticalAlignment: Text.AlignVCenter
						}

						onClicked: root.context.tryUnlock();
					}
				}

				Label {
					Layout.alignment: Qt.AlignHCenter
					visible: root.context.showFailure
					text: "✗ Incorrect password"
					color: root.errorColor
					font.family: "sans-serif"
					font.pointSize: 11
					font.weight: Font.Medium
				}

				Label {
					Layout.alignment: Qt.AlignHCenter
					visible: root.context.unlockInProgress
					text: "Authenticating..."
					color: root.accentColor
					font.family: "sans-serif"
					font.pointSize: 11
				}
			}
		}
	}

	// Hint text at bottom
	Label {
		anchors {
			bottom: parent.bottom
			horizontalCenter: parent.horizontalCenter
			bottomMargin: 40
		}
		text: "Press ESC or start typing to unlock"
		font.family: "sans-serif"
		font.pointSize: 10
		color: "#777777"
	}
}
