use bevy::prelude::*;
use sickle_ui::prelude::*;
pub struct ButtonPlugin;

impl Plugin for ButtonPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, (button_pseudo_state, handle_button_click));
        app.add_plugins(MyButtonPlugin);
    }
}

pub struct MyButtonPlugin;

impl Plugin for MyButtonPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(ComponentThemePlugin::<MyButton>::default());
    }
}

#[derive(Component, Clone, Debug, Default, Reflect, UiContext)]
#[reflect(Component)]
pub struct MyButton;

impl DefaultTheme for MyButton {
    fn default_theme() -> Option<Theme<Self>> {
        MyButton::theme().into()
    }
}

impl MyButton {
    pub fn theme() -> Theme<MyButton> {
        let base_theme = PseudoTheme::deferred(None, MyButton::primary_style);
        let pressed_theme = PseudoTheme::deferred(
            vec![PseudoState::Custom("Active".into())],
            MyButton::press_style,
        );
        Theme::new(vec![base_theme, pressed_theme])
    }

    fn primary_style(style_builder: &mut StyleBuilder, theme_data: &ThemeData) {
        let theme_spacing = theme_data.spacing;
        let colors = theme_data.colors();

        style_builder
            .padding(UiRect::all(Val::Px(theme_spacing.gaps.small)))
            .animated()
            .background_color(AnimatedVals {
                idle: colors.container(Container::SurfaceMid),
                hover: colors.container(Container::SurfaceHighest).into(),
                ..default()
            })
            .copy_from(theme_data.interaction_animation);
    }

    fn press_style(style_builder: &mut StyleBuilder, theme_data: &ThemeData) {
        let theme_spacing = theme_data.spacing;
        let colors = theme_data.colors();

        style_builder
            .background_color(colors.surface(Surface::Surface))
            .padding(UiRect::all(Val::Px(theme_spacing.gaps.small)));
    }

    fn frame() -> impl Bundle {
        (
            Name::new("My Widget"),
            ButtonBundle::default(),
            TrackedInteraction::default(),
        )
    }
}

pub trait UiMyButtonExt {
    fn my_button(
        &mut self,
        action: ButtonAction,
        spawn_children: impl FnOnce(&mut UiBuilder<Entity>),
    ) -> UiBuilder<Entity>;
}

impl UiMyButtonExt for UiBuilder<'_, Entity> {
    fn my_button(
        &mut self,
        action: ButtonAction,
        spawn_children: impl FnOnce(&mut UiBuilder<Entity>),
    ) -> UiBuilder<Entity> {
        self.container((MyButton::frame(), MyButton, action), spawn_children)
    }
}

#[derive(Bundle, Default)]
pub struct MyButtonBundle {
    pub button: ButtonBundle,
    pub interaction: TrackedInteraction,
    pub action: ButtonAction,
}

pub trait CloneFn: Sync + Send {
    fn call(&mut self, commands: &mut Commands);
    fn clone(&self) -> Box<dyn CloneFn>;
}

impl<F: FnMut(&mut Commands) + Clone + Send + Sync + 'static> CloneFn for F {
    fn call(&mut self, commands: &mut Commands) {
        self(commands);
    }

    fn clone(&self) -> Box<dyn CloneFn> {
        Box::new(self.clone())
    }
}

#[derive(Component)]
pub struct ButtonAction(Box<dyn CloneFn>);

impl ButtonAction {
    pub fn new(f: impl FnMut(&mut Commands) + Clone + Send + Sync + 'static) -> Self {
        Self(Box::new(f))
    }

    pub fn world(f: impl FnMut(&mut World) + Clone + Send + Sync + 'static) -> Self {
        Self(Box::new(
            for<'a, 'b, 'c> move |commands: &'a mut Commands<'b, 'c>| -> () {
                commands.add(f.clone());
            },
        ))
    }
}

impl Default for ButtonAction {
    fn default() -> Self {
        Self::new(for<'a, 'b, 'c> |_: &'a mut Commands<'b, 'c>| -> () {})
    }
}

fn button_pseudo_state(
    q: Query<(Entity, &FluxInteraction), (With<MyButton>, Changed<FluxInteraction>)>,
    mut commands: Commands,
) {
    for (e, interaction) in &q {
        match interaction {
            FluxInteraction::PointerEnter => {
                // commands.entity(e).add_pseudo_state(PseudoState::Custom("Hovered".into()));
            }
            FluxInteraction::PointerLeave => {
                // commands.entity(e).remove_pseudo_state(PseudoState::Custom("Hovered".into()));
            }
            FluxInteraction::Pressed => {
                commands
                    .entity(e)
                    .add_pseudo_state(PseudoState::Custom("Active".into()));
            }
            FluxInteraction::Released => {
                commands
                    .entity(e)
                    .remove_pseudo_state(PseudoState::Custom("Active".into()));
            }
            FluxInteraction::PressCanceled => {
                commands
                    .entity(e)
                    .remove_pseudo_state(PseudoState::Custom("Active".into()));
                // commands.entity(e).remove_pseudo_state(PseudoState::Custom("Hovered".into()));
            }
            _ => {}
        }
    }
}

fn handle_button_click(
    mut q: Query<(&FluxInteraction, &mut ButtonAction), Changed<FluxInteraction>>,
    mut commands: Commands,
) {
    for (interaction, mut action) in &mut q {
        if interaction.is_released() {
            action.0.call(&mut commands);
        }
    }
}
