from manim import *
from manim_slides import Slide, ThreeDSlide
import numpy as np
from scipy.interpolate import CubicSpline

# intro text scence
class IntroText(Slide):
    def construct(self):
        HWQ = Tex(
            '\"In these days, the angel of topology and the devil of abstract algebra',
            '   fight for the soul of every individual discipline of mathematics.\"').scale(0.5)
        HWQ.to_edge(UP)

        HW = Paragraph("- Herman Weyl", alignment="center", color=YELLOW).scale(0.75)
        HW.shift(UP * 2)

        self.play(FadeIn(HWQ, run_time = 3))

        self.play(Write(HW, run_time = 2))
        self.wait()

# paragraph 1
class IntroTorusAndKlein(ThreeDSlide):
    # paramtric functions
    def func(self, u, v):
        a = 2
        return np.array([(a + np.cos(v / 2) * np.sin(u) - np.sin(v / 2) * np.sin(2 * u)) * np.cos(v),
                         (a + np.cos(v / 2) * np.sin(u) - np.sin(v / 2) * np.sin(2 * u)) * np.sin(v),
                         np.sin(v / 2) * np.sin(u) + np.cos(v / 2) * np.sin(2 * u)])

    def construct(self):
        torus = Torus(major_radius = 2, minor_radius = 1, color=RED)
        klein = Surface(lambda u, v: self.func(u,v),
                        u_range=[0, 2 * PI],
                        v_range=[0,2 * PI])

        torus.shift(3.5 * DOWN)
        klein.shift(3.5 * UP)

        # Camera
        self.set_camera_orientation(theta=0 * DEGREES, phi = 40 * DEGREES)

        # Animations
        self.play(Create(torus), Create(klein), lag_ratio = 0, run_time = 5)
        self.wait()

        self.play(AnimationGroup(Rotate(klein, angle = 2 * PI, axis = OUT, rate_func = linear, run_time = 10),
                                 Rotate(torus, angle = 2 * PI, axis = OUT, rate_func = linear, run_time = 10)))
        self.wait()

class IntroLoops(Slide):
    def construct(self):
        # parametric functions
        # vertical ellipse
        def func_1(t):
            return np.array([0.75 * np.cos(2 * PI * t), np.sin(2 * PI * t), 0])

        # set up for keyhole loop
        func_2_points = [[-5,0,0], [-3.5,-2,0], [0,-1,0], [3.5,-2,0], [5,0,0], [3.5, 2,0], [0,1,0], [-3.5,2,0], [-5,0,0]]

        # Extract the x and y coordinates from the points
        x_coords = [point[0] for point in func_2_points]
        y_coords = [point[1] for point in func_2_points]

        # Create a cubic spline interpolator for x and y with periodic boundary conditions
        spline_x = CubicSpline(range(len(x_coords)), x_coords, bc_type='periodic')
        spline_y = CubicSpline(range(len(y_coords)), y_coords, bc_type='periodic')

        # keyhole loop parametrization
        def func_2(t):
            return np.array([spline_x(t), spline_y(t), 0])

        # loop definitions
        loop_1 = ParametricFunction(
            func_1,
            t_range = (0,1),
            color = RED,
            stroke_width = 4
        ).shift(5.75 * LEFT)

        loop_2 = ParametricFunction(
            func_2,
            t_range = (0, len(func_2_points) - 1),  # Parameter range
            color = RED,
            stroke_width = 4
        )

        # top space
        blob = Polygon([-5,0,0], [-3.5,-2,0], [0,-1,0], [3.5,-2,0], [5,0,0], [3.5, 2,0], [0,1,0], [-3.5,2,0]).round_corners(radius = 1.5).scale(1.5)
        small_blob = Polygon([-5,0,0], [-3.5,-2,0], [0,-1,0], [3.5,-2,0], [5,0,0], [3.5, 2,0], [0,1,0], [-3.5,2,0]).round_corners(radius = 1.5).scale(0.5)
        c = Cutout(blob, small_blob, fill_opacity=1, color=BLUE_D, stroke_color=WHITE)

        # Base Point
        base = Dot(radius = 0.07, color = RED)
        base.move_to(5 * LEFT)

        # Animations
        self.play(FadeIn(c), run_time=4, rate_func=linear)
        self.wait()

        self.play(FadeIn(base))
        self.wait()

        self.play(Create(loop_1), run_time=3, rate_func=linear)
        self.wait()

        self.play(Create(loop_2), run_time=4, rate_func=linear)
        self.wait()

        self.play(Uncreate(loop_2), run_time=4, rate_func=linear)
        self.wait()

        self.play(Uncreate(loop_1), run_time=3, rate_func=linear)
        self.wait()


class IntroCoveringSpaces(ThreeDSlide):
    def construct(self):
        # set up for blob
        points = [[0,1,0], [-0.75,0.5,0], [-1,0,0], [-1,-0.5,0], [-0.33,-0.7,0],
                  [0,-0.8,0], [0.28,-0.73,0],
                  [0.71,0.25,0], [1,0.5,0], [0,1,0]]
        # Extract the x and y coordinates from the points
        x_coords = [point[0] for point in points]
        y_coords = [point[1] for point in points]

        # Create a cubic spline interpolator for x and y with periodic boundary conditions
        spline_x = CubicSpline(range(len(x_coords)), x_coords, bc_type='periodic')
        spline_y = CubicSpline(range(len(y_coords)), y_coords, bc_type='periodic')

        # blob function
        def func(t):
            return np.array([spline_x(t), spline_y(t), 0])

        # create the vertices for the blob using the parametric function
        vertices = [func(t) for t in np.linspace(0, len(points) - 1, 100)]

        # Create the filled blob
        blob = VMobject()
        blob.set_points_as_corners(vertices + [vertices[0]])  # Ensure it's a closed shape
        blob.set_fill(color=BLUE, opacity=0.5)  # Set fill color and opacity
        blob.set_stroke(color=WHITE, width=2)  # Optional: set stroke color and width

        base_blob = blob.copy().move_to(ORIGIN).shift(OUT * -2.65)

        # create pancake stacks
        num_stacks = 6
        stacks = VGroup()
        for i in range(num_stacks):
            stack = blob.copy()
            stack.shift(OUT * i * 0.67)
            stacks.add(stack)

        # Top space
        top_space = Ellipse(width = 7, height = 6, color = BLACK).move_to(ORIGIN).shift(OUT * -2.65).set_fill(WHITE, opacity = 0.7)

        # TeX
        down_arrow = Tex(r"$\downarrow$").scale(2).rotate(angle = PI / 2, axis = RIGHT).rotate(angle = -1 * PI / 4, axis = OUT).shift(IN)

        # Animations
        self.move_camera(phi=75 * DEGREES, theta=30 * DEGREES)
        self.play(*[FadeIn(stack) for stack in stacks])
        self.play(FadeIn(top_space), FadeIn(base_blob), lag_ratio = 0)
        self.play(FadeIn(down_arrow))
        self.wait()


class BaselineAssumptions(Slide):
    def construct(self):
        baseline_assumtions = Tex(r"\underline{Baseline Assumptions}").to_edge(UP).scale(2)

        itemized_group = Tex(r"\begin{itemize}\item Some Basic Set Theory \item Groups \item Topological Spaces \end{itemize}")

         # Arrange items vertically
        itemized_group.arrange(DOWN, buff=0.5)

        # Position the group in the scene
        itemized_group.move_to(ORIGIN).shift(LEFT + UP)

        # Animations
        self.play(FadeIn(baseline_assumtions), FadeIn(itemized_group))


############# PART 1 : PATHS AND HOMOTOPY
class P1HomotopyIntro(ThreeDSlide):
    def construct(self):
        self.set_camera_orientation(phi=75 * DEGREES, theta=0 * DEGREES)

        r1 = 1
        h = ValueTracker(0)
        r2 = ValueTracker(r1)

        def surfFunc(u,v):
            z = v*h.get_value()
            r = r1 + v*(r2.get_value()-r1)
            return np.array([r*np.cos(u),r*np.sin(u),z])
        surf = always_redraw(lambda:
            Surface(
                func = surfFunc,
                u_range=[0,2*PI],
                v_range=[0,1],
                resolution=[16,16]
            )
        )
        self.add(surf)
        self.wait()
        self.play(
            h.animate.set_value(3),
            run_time=5
        )
        self.wait()
        self.play(
            h.animate.set_value(0),
            r2.animate.set_value(4.5),
            run_time=5
        )
        self.wait()
        self.play(
            r2.animate.set_value(r1),
            run_time=5
        )
        self.wait()

#TODO: fix timing
class P1HomotopyDef(Slide):
    def construct(self):
        d = Tex(r"\textsc{Definition}").to_edge(UL).scale(0.75)
        d_box = SurroundingRectangle(d, color=WHITE, buff=0.2)

        homotopy = Tex(r"Given two spaces $X$ and $Y$, a \textbf{\textit{homotopy}} from $X$ to $Y$ is a \underline{family of maps} ",
                 r"$h_t : X \to Y$ indexed by $t \in I$ which is continuous in the sense that the associated function $H : X \times I \to Y$ given by $H(x,t) = h_t(x)$ is continuous.")
        homotopy.scale(0.7).shift(1.5 * UP)


        space_X = SVGMobject("./svgs/genus_2.svg").scale(1.75).shift(2 * DOWN + 3.25 * LEFT).set_stroke(color=WHITE, width=2)
        space_Y = SVGMobject("./svgs/4_wedge_sum.svg").scale(1.5).shift(2 * DOWN + 3.25 * RIGHT).set_stroke(color=WHITE, width=2)

        label_X = MathTex("X").move_to(space_X.get_top()).shift(0.5 * UP)
        label_Y = MathTex("Y").move_to(space_Y.get_top()).shift(0.5 * UP)

        spaces = VGroup(space_X, space_Y)
        labels = VGroup(label_X, label_Y)

        arrow = Line(start=space_X.get_right(), end=space_Y.get_left(), buff=0.75).add_tip()

        label_ht = Tex(r"$h_t$").move_to(arrow.get_top()).shift(0.2 * UP)

        self.play(FadeIn(VGroup(d, d_box)))

        self.play(FadeIn(homotopy))

        self.play(FadeIn(labels))
        self.play(DrawBorderThenFill(spaces))

        self.play(Create(arrow), FadeIn(label_ht), lag_ratio=0)
        self.wait()

class P1HomotopicDef(Slide):
    def construct(self):
        d = Tex(r"\textsc{Definition}").to_edge(UL).scale(0.75)
        d_box = SurroundingRectangle(d, color=WHITE, buff=0.2)

        homotopic = Tex(r"Two functions",
                 r"$\,f$",
                 r",",
                 r"$\,g$",
                 r"$\,: X\to Y$ are said to be \textbf{\textit{homotopic}} if there is a homotopy $h_t : X\to Y$ such that $h_0=$",
                 r"$\,f$",
                 r" and $h_1=$",
                 r"$\,g$",
                 r" and we write ",
                 r"$f$",
                 r"\,$\simeq$",
                 r"\,$g$",
                 r".")

        homotopic.scale(0.7).shift(1.5 * UP)
        homotopic[1].set_color(RED)
        homotopic[3].set_color(BLUE)
        homotopic[5].set_color(RED)
        homotopic[7].set_color(BLUE)
        homotopic[9].set_color(RED)
        homotopic[11].set_color(BLUE)

        curve1 = ParametricFunction(lambda t:
                                    np.array([t, t**3, 0]),
                                    t_range=(-1,1),
                                    color=RED
                                    )


        curve2 = ParametricFunction(lambda t:
                                    np.array([t, t * np.sin(7 / t), 0]),
                                    t_range=(-1,1),
                                    color=BLUE_E
                                    )

        curves = VGroup(curve1, curve2)

        curves.shift(2 * DOWN).scale(1.75)

        self.add(VGroup(d,d_box))

        self.play(FadeIn(homotopic))
        self.wait()

        self.play(Create(curves), run_time=3)
        self.wait(3)

        self.play(Transform(curve2, curve1, rate_func=linear, run_time=5))
        self.wait()

class P1HomotopyEquivalenceDef(Slide):
    def construct(self):
        d = Tex(r"\textsc{Definition}").to_edge(UL).scale(0.75)
        d_box = SurroundingRectangle(d, color=WHITE, buff=0.2)

        homEq = Tex(r"If there are maps $f:X\to Y$ and $g:Y\to X$ such that $g\circ f\simeq \text{id}_X$ and $f\circ g\simeq \text{id}_Y$, then $f$ and $g$ are called \textbf{\textit{homotopy equivalences}} and $X$ and $Y$ are said to be \textbf{\textit{homotopy equivalent}}.")
        homEq.scale(0.7).shift(1.5 * UP)

        r = ValueTracker(1)
        a = ValueTracker(3)

        disk = always_redraw(lambda: Circle(radius=r.get_value(), color=WHITE, fill_opacity=1).shift(2 * DOWN + a.get_value() * LEFT))
        center = always_redraw(lambda: Dot(radius=0.1, color=RED).move_to(disk.get_center()))
        label_X = MathTex("X").move_to(disk.get_top()).shift(0.5 * UP)

        domain = VGroup(disk, label_X)

        point = Dot(radius=0.1, color=RED).shift(2 * DOWN + 3 * RIGHT)
        label_Y = MathTex("Y").move_to(point.get_right()).shift(0.5 * RIGHT)
        codomain = VGroup(point, label_Y)

        curvedArrow_f = CurvedArrow(start_point=disk.get_corner(UR), end_point=point.get_corner(UL), angle=-PI/4)
        label_f = Tex(r"$f$").move_to(curvedArrow_f.get_top()).shift(0.2 * UP)
        arrowGroup_f = VGroup(curvedArrow_f, label_f)

        curvedArrow_g = CurvedArrow(start_point=point.get_corner(DL), end_point=disk.get_corner(DR), angle=-PI/4)
        label_g = Tex(r"$g$").move_to(curvedArrow_g.get_bottom()).shift(0.2 * DOWN)
        arrowGroup_g = VGroup(curvedArrow_g, label_g)

        self.add(VGroup(d,d_box))
        self.play(FadeIn(homEq))
        self.wait()
        self.play(FadeIn(domain))
        self.play(FadeIn(codomain))
        self.play(FadeIn(arrowGroup_f))
        self.play(FadeIn(arrowGroup_g), FadeIn(center))

        group3 = VGroup(label_X, arrowGroup_f, arrowGroup_g, codomain)
        self.play(FadeOut(group3))

        self.play(a.animate.set_value(0))

        self.play(r.animate.set_value(0.05))
        self.play(r.animate.set_value(1.2))
        self.play(r.animate.set_value(0.05))
        self.wait()


#TODO: Fix timing
class P1PathHomtopyDef(Slide):
    def construct(self):
        blob = SVGMobject("./svgs/blob.svg").set_stroke(color=WHITE, width=4).scale(2.75).to_edge(DOWN)

        x_0 = Dot(radius=0.1, color=WHITE).move_to(2 * LEFT + DOWN)
        x_1 = Dot(radius=0.1, color=WHITE).move_to(2 * RIGHT + DOWN)

        label_0 = MathTex("x_0").move_to(x_0.get_left()).shift(0.5 * LEFT)
        label_1 = MathTex("x_1").move_to(x_1.get_right()).shift(0.5 * RIGHT)

        def linear_homotopy(x,y,z,t):
            return (x, y * (1-t) + t * x_0.get_y(), z)

        curve = ParametricFunction(lambda t:
                                  np.array([(x_0.get_x() + 0.1) + t * ((x_1.get_x() - 0.1) - (x_0.get_x() + 0.1)), np.sin(2 * PI * t) + x_0.get_y(), 0]),
                                  t_range=(0,1),
                                  color=RED
                                  )
        line = ParametricFunction(lambda t:
                                  np.array([(x_0.get_x() + 0.1) + t * ((x_1.get_x() - 0.1) - (x_0.get_x() + 0.1)), x_0.get_y(), 0]),
                                  t_range=(0,1),
                                  color=BLUE
                                  )

        h_0 = MathTex(r"h_0", r"\, : I \to X").to_edge(UP)
        h_0[0].set_color(RED)

        h_1 = MathTex(r"h_1", r"\, : I \to X").to_corner(UR)
        h_1[0].set_color(BLUE)

        self.play(Create(blob), run_time=2)
        self.play(FadeIn(VGroup(x_0,x_1, label_0, label_1)))


        self.play(FadeIn(h_0), Create(curve, run_time=2))
        self.wait()

        self.play(h_0.animate.to_corner(UL))

        self.play(FadeIn(h_1))
        self.play(Create(line, run_time=2))
        self.wait()

        self.play(Homotopy(linear_homotopy, curve, run_time=3.5))

class P1HomtopyEquivalenceRelation(Slide):
    def construct(self):
        lemma = Tex(r"\textsc{Lemma}").to_edge(UL).scale(0.75)
        lemma_box = SurroundingRectangle(lemma, color=WHITE, buff=0.2)

        title = Tex(r"The (path)-homotopy relation is an equivalence relation").to_edge(UP).shift(DOWN).scale(0.75)

        proof = Tex(r"\begin{itemize} \item \textbf{Reflexivity:} $\gamma \simeq \gamma$ \item \textbf{Symmetry:} $\gamma\simeq \gamma' \implies \gamma'\simeq\gamma$ \item \textbf{Transitivity:} $\gamma\simeq \gamma', \gamma'\simeq\gamma''\implies\gamma\simeq\gamma''$\end{itemize}")
        proof.scale(0.75).move_to(ORIGIN).shift(0.5 * UP)

        eq_rel = Tex(r"The equivalence class of a map $\gamma$, $[\gamma]$, is all the maps (path-)homotopic to $\gamma$").scale(0.75).move_to(proof.get_bottom()).shift(DOWN)

        self.play(FadeIn(VGroup(lemma, lemma_box)))
        self.play(FadeIn(title))
        self.play(FadeIn(proof))
        self.wait()
        self.play(FadeIn(eq_rel))

        self.wait()


#TODO: Fix timing
class P1Concatenation(Slide):
    def construct(self):
        definition = Tex(r"\textsc{Definition}").to_edge(UL).scale(0.75)
        def_box = SurroundingRectangle(definition, color=WHITE, buff=0.2)


        concatenation_def = Tex(r"Two paths $\gamma,\gamma' : I \to X$ are \textbf{\textit{composable}} if $\gamma(1) = \gamma'(0)$").scale(0.75).to_edge(UP).shift(DOWN)
        function = Tex(r"Their \textbf{\textit{concatenation}} is the map $\gamma\ast\gamma' : I \to X$").scale(0.75).move_to(concatenation_def.get_bottom()).shift(0.75 * DOWN)
        concatenation = MathTex(r"\gamma\ast\gamma'(s) = \begin{cases}\gamma(2s) & \text{ if } s \in [0,1/2] \\ \gamma'(2s - 1) & \text{ if }s \in [1/2,1]\end{cases}").move_to(ORIGIN).scale(0.75)

        y = 2
        a = 4
        curve_1 = ParametricFunction(lambda t:
                                     np.array([-1 * a * (1-t), np.sin(2 * PI * t) - y, 0]),
                                     t_range=(0,1),
                                     color=RED
                                     )

        curve_2 = ParametricFunction(lambda t:
                                     np.array([a * t, -1 * np.sin(2 * PI * t) - y, 0]),
                                     t_range=(0,1),
                                     color=BLUE
                                     )

        common = Dot(radius=0.1, color=WHITE).move_to(y * DOWN)

        point = Dot(radius=0.1, color=PURPLE).move_to(a * LEFT + y * DOWN)


        self.play(FadeIn(VGroup(definition, def_box)))
        self.wait()

        self.play(FadeIn(concatenation_def))
        self.wait()

        self.play(FadeIn(function))
        self.wait()

        self.play(FadeIn(concatenation))
        self.wait()

        self.play(Create(VGroup(curve_1, curve_2)))
        self.wait()

        self.play(FadeIn(common))
        self.wait()

        # Move the point along curve_1
        self.play(MoveAlongPath(point, curve_1))

        # Move the point along curve_2
        self.play(MoveAlongPath(point, curve_2))
        self.wait()

#TODO: Fix timing and add bit about identity element, inverse elements
class P1FundamentalGroup(Slide):
    def construct(self):
        d = Tex(r"\textsc{Definition}").to_edge(UL).scale(0.75)
        def_box = SurroundingRectangle(d, color=WHITE, buff=0.2)

        loop = Tex(r"A \textbf{\textit{loop}} in a space $X$ is a path $\gamma : I \to X$ with $\gamma(0) = \gamma(1)$.").scale(0.75).to_edge(UP).shift(DOWN)

        def_fg = Tex(r"Given a pointed space $(X,x_0)$, the \textbf{\textit{fundamental group}} of $X$ based at $x_0$ is the group").scale(0.75).move_to(loop.get_bottom()).shift(DOWN)

        fg = MathTex(r"\pi_1(X,x_0) = \{[\gamma] : \gamma \text{ is a loop in $X$ based at $x_0$}\}").scale(0.75).move_to(ORIGIN)

        operation = Tex(r"with operation $[\gamma][\gamma']=[\gamma\ast\gamma']$").scale(0.75).move_to(fg.get_bottom()).shift(DOWN)

        self.play(FadeIn(VGroup(d,def_box)))

        self.play(FadeIn(loop))
        self.wait()

        self.play(FadeIn(VGroup(def_fg, fg, operation)))
        self.wait()

class P1FundamentalGroupBasePoint(Slide):
    def construct(self):
        question = Tex(r"Question: Does the fundamental group of a space depend on the choice of base point?").scale(0.75)

        lemma = Tex(r"\textsc{Lemma}").to_edge(UL).scale(0.75)
        lemma_box = SurroundingRectangle(lemma, color=WHITE, buff=0.2)

        l = Tex(r"Given pointed spaces $(X,x_0)$ and $(X,x_1)$ with $x_0$ and $x_1$ in the same path component, $\pi_1(X,x_0)\simeq\pi_1(X,x_1)$").scale(0.75).to_edge(UP).shift(DOWN)


        blob = SVGMobject("./svgs/blob.svg").scale(2.45).set_stroke(color=WHITE, width=4).to_edge(DOWN)

        x_0 = Dot(radius=0.1, color=WHITE).move_to(2 * LEFT + DOWN)
        x_1 = Dot(radius=0.1, color=WHITE).move_to(2 * RIGHT + DOWN)

        label_0 = MathTex("x_0").move_to(x_0.get_left()).shift(0.5 * LEFT)
        label_1 = MathTex("x_1").move_to(x_1.get_right()).shift(0.5 * RIGHT)

        points_0 = [[-2,-0.9,0], [-1.66, -0.25, 0], [-1, 0.5, 0], [-0.85, 0, 0],[-0.75,-1,0], [-1.9,-1,0]]

        # Extract the x and y coordinates from the points
        x_coords_0 = [point[0] for point in points_0]
        y_coords_0 = [point[1] for point in points_0]

        # Create a cubic spline interpolator for x and y with periodic boundary conditions
        spline_x_0 = CubicSpline(range(len(x_coords_0)), x_coords_0, extrapolate=False)
        spline_y_0 = CubicSpline(range(len(y_coords_0)), y_coords_0, extrapolate=False)

        # blob function
        def func_1(t):
            return np.array([spline_x_0(t), spline_y_0(t), 0])

        # create the vertices for the blob using the parametric function
        vertices_0 = [func_1(t) for t in np.linspace(0.05, len(points_0) - 1 - 0.001, 1000)]

        # Create the filled blob
        curve_0 = VMobject()
        curve_0.set_points_as_corners(vertices_0)  # Ensure it's a closed shape
        curve_0.set_stroke(color=RED, width=3)  # Optional: set stroke color and width


        curve_1 = ParametricFunction(lambda t:
                                     np.array([x_1.get_x() + t * (x_0.get_x() - x_1.get_x()),
                                               np.sin(2 * PI * t) + x_1.get_y(),
                                               0]),
                                     t_range=(0.0125,1 - 0.0125),
                                     color=BLUE
                                     )

        point = Dot(radius=0.1, color=PURPLE).move_to(x_1.get_x() * RIGHT + x_1.get_y() * UP)

        self.play(FadeIn(question))
        self.wait()
        self.play(FadeOut(question))

        self.play(FadeIn(VGroup(lemma, lemma_box)))

        self.play(FadeIn(l))
        self.wait()

        self.play(FadeIn(blob))
        self.play(FadeIn(VGroup(x_0, label_0, x_1, label_1)))

        self.play(Create(curve_0))
        self.play(Create(curve_1))
        self.play(MoveAlongPath(point, curve_1))
        self.play(MoveAlongPath(point, curve_0))
        self.play(MoveAlongPath(point, curve_1), rate_func=lambda t: 1-t)

# Repeat of P1HomotopyIntro
class P1HomotopyPreservesHoles(ThreeDSlide):
    def construct(self):
        self.set_camera_orientation(phi=75 * DEGREES, theta=0 * DEGREES)

        r1 = 1
        h = ValueTracker(0)
        r2 = ValueTracker(r1)

        def surfFunc(u,v):
            z = v*h.get_value()
            r = r1 + v*(r2.get_value()-r1)
            return np.array([r*np.cos(u),r*np.sin(u),z])
        surf = always_redraw(lambda:
            Surface(
                func = surfFunc,
                u_range=[0,2*PI],
                v_range=[0,1],
                resolution=[16,16]
            )
        )
        self.add(surf)
        self.wait()
        self.play(
            h.animate.set_value(3),
            run_time=5
        )
        self.wait()
        self.play(
            h.animate.set_value(0),
            r2.animate.set_value(4.5),
            run_time=5
        )
        self.wait()
        self.play(
            r2.animate.set_value(r1),
            run_time=5
        )
        self.wait()

class P1ContractibleSpace(Slide):
    def construct(self):
        d = Tex(r"\textsc{Definition}").to_edge(UL).scale(0.75)
        def_box = SurroundingRectangle(d, color=WHITE, buff=0.2)

        contractible = Tex(r"A \textbf{\textit{contractible}} space is a space which has the same homotopy type as a point").scale(0.75).to_edge(UP).shift(DOWN)
        map = MathTex(r"h_t : X \to \{\ast\}").to_edge(UP).shift(2 * DOWN)

        r = ValueTracker(2)

        disk = always_redraw(lambda: Circle(radius=r.get_value(), color=WHITE, fill_opacity=1).shift(1.75 * DOWN))

        self.play(FadeIn(VGroup(d,def_box)))
        self.wait()

        self.play(FadeIn(contractible), FadeIn(map))
        self.wait()

        self.play(FadeIn(disk))
        self.wait()

        self.play(r.animate.set_value(0.05))
        self.wait()

class P1SimplyConnected(Slide):
    def construct(self):
        d = Tex(r"\textsc{Definition}").to_edge(UL).scale(0.75)
        def_box = SurroundingRectangle(d, color=WHITE, buff=0.2)

        simply_connected = Tex(r"A space \textbf{\textit{simply-connected}} space is one with trivial fundamental group").scale(0.75).to_edge(UP).shift(DOWN)

        pseudo_circle = SVGMobject("./svgs/pseudocircle.svg").set_stroke(color=WHITE).set_fill(color=WHITE).scale(2).shift(1.75 * DOWN)

        self.play(FadeIn(VGroup(d,def_box)))
        self.wait(10)

        self.play(FadeIn(simply_connected))
        self.wait()

        self.play(Create(pseudo_circle), run_time=3)
        self.wait()

################# PART 2: Fundamental Group of the Circle
class P2CoveringSpaceDemo(ThreeDSlide):
    def construct(self):
        # Major radius
        R = 2

        # Minor radius
        r = 1

        # Number of loops around torus
        a = 3

        # Create the torus object
        torus = Torus(major_radius=R, minor_radius=r).set_opacity(0.6)

        # Create the curve as a ParametricFunction
        curve1 = ParametricFunction(lambda t:
                                    np.array([(R + r * np.cos(t)) * np.cos(a * t),
                                              (R + r * np.cos(t)) * np.sin(a * t),
                                              r * np.sin(t)]),
                                    t_range=(0, 2 * PI / 3),
                                    stroke_width=3,
                                    stroke_opacity=0.75,
                                    stroke_color=RED
                                   )
        curve2 = ParametricFunction(lambda t:
                                    np.array([(R + r * np.cos(t)) * np.cos(a * t),
                                              (R + r * np.cos(t)) * np.sin(a * t),
                                              r * np.sin(t)]),
                                    t_range=(2 * PI / 3, 4 * PI / 3),
                                    stroke_width=3,
                                    stroke_opacity=0.75,
                                    stroke_color=GREEN
                                   )
        curve3 = ParametricFunction(lambda t:
                                    np.array([(R + r * np.cos(t)) * np.cos(a * t),
                                              (R + r * np.cos(t)) * np.sin(a * t),
                                              r * np.sin(t)]),
                                    t_range=(4 * PI / 3, 2 * PI),
                                    stroke_width=3,
                                    stroke_opacity=0.75,
                                    stroke_color=YELLOW
                                   )

        # Create a circle that the torus wraps around
        circle = ParametricFunction(lambda t:
                                    np.array([0.5 * (R + r) * np.cos(t),
                                              0.5 * (R + r) * np.sin(t),
                                              0]),
                                    t_range=(0, 2 * PI)
                                    ).set_stroke(color=WHITE, width=3)

        # Set the camera orientation
        self.set_camera_orientation(phi=75 * DEGREES, theta=0 * DEGREES)

        # Add objects to the scene
        self.play(Create(torus))
        self.wait()

        self.play(Create(circle), run_time=2)
        self.wait()

        # Animate the ValueTracker to change the color of the curve
        self.play(Create(curve1), run_time=2, rate_func=linear)
        self.play(Create(curve2), run_time=2, rate_func=linear)
        self.play(Create(curve3), run_time=2, rate_func=linear)
        self.wait(4)

        self.play(Rotate(VGroup(torus, curve1, curve2, curve3, circle), angle=2 * PI), rate_func=linear, run_time=10)
        self.wait()



class P2Lift(Slide):
    def construct(self):
        latex_temp = TexTemplate()
        latex_temp.add_to_preamble(r"\usepackage{tikz-cd}")
        latex_temp.add_to_preamble(r"\usepackage{quiver}")

        d = Tex(r"\textsc{Definition}").to_edge(UL).scale(0.75)
        d_box = SurroundingRectangle(d, color=WHITE, buff=0.2)

        lift = Tex(r"Given maps $f : X \to Z$ and $g : Y \to Z$, a map $h : X \to Y$ is called a \textbf{\textit{lift}} of $f$ if $f = g\circ h$, and we say that $f$ factors through $g$ (by $h$)").scale(0.75).to_edge(UP).shift(DOWN)

        diagram = Tex(r"""
            \begin{tikzcd}[ampersand replacement=\&]
	            \& Y \\
	            X \& Z
	            \arrow["g", from=1-2, to=2-2]
	            \arrow["h", from=2-1, to=1-2]
	            \arrow["f"', from=2-1, to=2-2]
            \end{tikzcd}""",
                      tex_template=latex_temp).move_to(ORIGIN).set_stroke(width=1).scale(0.75)


        self.play(FadeIn(VGroup(d,d_box)))
        self.wait()

        self.play(FadeIn(lift))
        self.wait()

        self.play(FadeIn(diagram))
        self.wait()


class P2EvenlyCoveredDef(Slide):
    def construct(self):
        d = Tex(r"\textsc{Definition}").to_edge(UL).scale(0.75)
        def_box = SurroundingRectangle(d, color=WHITE, buff=0.2)

        evenly_covered = Tex(r"An open subset $U$ of a space $X$ is \textbf{\textit{evenly covered}} by a map $p : \widetilde{X} \to X$ if $p^{-1}(U) = \bigsqcup_{\alpha}V_{\alpha}$ is the disjoint union of open sets $V_{\alpha}$ which project homeomorphically onto $U$ $$p\circ\iota_{\alpha}  :V_{\alpha}\to U$$")
        evenly_covered.scale(0.75).to_edge(UP).shift(DOWN)

        self.play(FadeIn(VGroup(d,def_box)))
        self.wait()

        self.play(FadeIn(evenly_covered))
        self.wait()



class P2EvenlyCoveredVisual(ThreeDSlide):
    def construct(self):
        # set up for blob
        points = [[0,1,0], [-0.75,0.5,0], [-1,0,0], [-1,-0.5,0], [-0.33,-0.7,0],
                  [0,-0.8,0], [0.28,-0.73,0],
                  [0.71,0.25,0], [1,0.5,0], [0,1,0]]
        # Extract the x and y coordinates from the points
        x_coords = [point[0] for point in points]
        y_coords = [point[1] for point in points]

        # Create a cubic spline interpolator for x and y with periodic boundary conditions
        spline_x = CubicSpline(range(len(x_coords)), x_coords, bc_type='periodic')
        spline_y = CubicSpline(range(len(y_coords)), y_coords, bc_type='periodic')

        # blob function
        def func(t):
            return np.array([spline_x(t), spline_y(t), 0])

        # create the vertices for the blob using the parametric function
        vertices = [func(t) for t in np.linspace(0, len(points) - 1, 100)]

        # Create the filled blob
        blob = VMobject()
        blob.set_points_as_corners(vertices + [vertices[0]])  # Ensure it's a closed shape
        blob.set_fill(color=BLUE, opacity=0.5)  # Set fill color and opacity
        blob.set_stroke(color=WHITE, width=2)  # Optional: set stroke color and width

        base_blob = blob.copy().move_to(ORIGIN).shift(OUT * -2.65)

        # create pancake stacks
        num_stacks = 6
        stacks = VGroup()
        for i in range(num_stacks):
            stack = blob.copy()
            stack.shift(OUT * i * 0.67)
            stacks.add(stack)

        # Top space
        top_space = Ellipse(width = 7, height = 6, color = BLACK).move_to(ORIGIN).shift(OUT * -2.65).set_fill(WHITE, opacity = 0.7)
        label_X = MathTex("X").move_to(top_space.get_corner(UL)). rotate(angle=PI/4, axis=RIGHT)

        # TeX
        down_arrow = Tex(r"$\downarrow$").scale(2).rotate(angle = PI / 2, axis = RIGHT).rotate(angle = 0, axis = OUT).shift(IN)
        p = MathTex("p").move_to(down_arrow.get_left()).rotate(angle=PI/4, axis=RIGHT).shift(0.5 * LEFT)

        stacks_label = MathTex("p^{-1}(U)").move_to(stacks[num_stacks - 1].get_corner(UL)).rotate(angle=PI/4, axis=RIGHT).shift(1.5 * LEFT)

        # Animations
        self.set_camera_orientation(phi=75 * DEGREES, theta=-90 * DEGREES)
        self.play(*[FadeIn(stack) for stack in stacks])
        self.play(FadeIn(VGroup(top_space, label_X)), FadeIn(base_blob), lag_ratio = 0)
        self.play(FadeIn(VGroup(p,down_arrow)), FadeIn(stacks_label))
        self.wait()

class P2CoveringSpaceDef(Slide):
    def construct(self):
        d = Tex(r"\textsc{Definition}").to_edge(UL).scale(0.75)
        def_box = SurroundingRectangle(d, color=WHITE, buff=0.2)

        covering_space = Tex(r"Given a space $X$, a \textbf{\textit{covering space}} of $X$ is a space $\widetilde{X}$ with a map $p : \widetilde{X}\to X$ such that each point of $X$ has an evenly covered neighborhood by $p$")
        covering_space.scale(0.75).to_edge(UP).shift(DOWN)

        self.play(FadeIn(VGroup(d,def_box)))
        self.wait()

        self.play(FadeIn(covering_space))
        self.wait()

class P2HLPDef(Slide):
    def construct(self):
        latex_temp = TexTemplate()
        latex_temp.add_to_preamble(r"\usepackage{tikz-cd}")
        latex_temp.add_to_preamble(r"\usepackage{quiver}")

        d = Tex(r"\textsc{Definition}").to_edge(UL).scale(0.75)
        d_box = SurroundingRectangle(d, color=WHITE, buff=0.2)

        hlp = Tex(r"""
                    Given a covering space $p : \widetilde{X}\to X$, a homotopy $h_t : Y \to X$, and a map
                    $\widetilde{h}_0 : Y\to \widetilde{X}$ lifting $f_0$, then there exists a unique homotopy
                    $\widetilde{h}_t : Y \to \widetilde{X}$ of $\widetilde{h}_0$ that lifts $h_t$."""
                  ).scale(0.75).to_edge(UP).shift(DOWN)

        diagram = Tex(r"""
                        \begin{tikzcd}[ampersand replacement=\&]
                            Y \&\& {\widetilde{X}} \\
                            {Y \times I} \&\& X
                            \arrow["{\widetilde{h}_0}", from=1-1, to=1-3]
                            \arrow["{\iota_0}"', hook, from=1-1, to=2-1]
                            \arrow["p", from=1-3, to=2-3]
                            \arrow["{\exists!\widetilde{h}_t}", dashed, from=2-1, to=1-3]
                            \arrow["{h_t}", from=2-1, to=2-3]
                        \end{tikzcd}""",
                      tex_template=latex_temp).move_to(ORIGIN).set_stroke(width=1).shift(DOWN)

        self.play(FadeIn(VGroup(d,d_box)))
        self.wait()

        self.play(FadeIn(hlp))
        self.wait()

        self.play(FadeIn(diagram))
        self.wait()

class P2RCoversS1(ThreeDSlide):
    def construct(self):
        def helix_func(t):
            return np.array([np.cos(4 * t), np.sin(4 * t), t])
        helix = ParametricFunction(helix_func, t_range=(0, 2 * PI),stroke_width=2)
        cylinder = Cylinder(radius=1, height=2 * PI).set_opacity(0.3).move_to(helix.get_center())

        circle = Circle(radius=1).move_to(cylinder.get_center()).shift(5 * IN)

        vdots_1 = MathTex(r"\vdots")
        vdots_2 = MathTex(r"\vdots")

        #TODO: Place vdots above and below cylinder

        self.set_camera_orientation(phi=75*DEGREES, theta=0*DEGREES, zoom=0.75)
        self.play(Create(circle))
        self.play(Create(cylinder), run_time=5)
        self.play(Create(helix), run_time=10, rate_func=linear)










