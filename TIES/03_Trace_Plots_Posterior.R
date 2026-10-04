################################################################################
######################### Application LCPA - TIES Data #########################
##################### Trace Plots - LCPA MCMC Convergence ######################
################################################################################

library(ggplot2)
library(tidyverse)

# Use .rds file from 02.Application.R or use original file from folder "Data"

LCPA <- readRDS("LCPA_SAN_50000.rds")

N <- LCPA$iteration$niter
chains <- LCPA$param.chain

# =============================================================================
# 1. TRACE PLOT: gam.pf 
# ============================================================================= 
gam_trace <- map_dfr(seq_along(chains$lcpa$gam.pf), function(iter) {
  mat <- chains$lcpa$gam.pf[[iter]]
  as.data.frame.table(mat, responseName="value") %>%
    mutate(iteration = iter)
}) %>%
  mutate(
    Profile = gsub("Profile\\.", "Profile ", as.character(Var1)),
    Group   = gsub("Group\\.", "Group ", as.character(Var2))
  ) 

ggplot(gam_trace, aes(x=iteration, y=value, color=Group, group=interaction(Profile, Group))) +
  geom_line(alpha=0.7, linewidth=0.5) +
  facet_wrap(~Profile, nrow=1) +
  labs(title=paste("Trace Plot: Profile Sizes (gam.pf, N=",N,")",sep=""),
       x="Iteration", y="Proportion", color="Group") +
  theme_minimal(base_size=12) +
  theme(strip.text=element_text(face="bold"))

ggsave(paste("SAN_50000/Diagnostics/Trace_plot_gamma","_N",N,".png",sep=""),
       width = 15,           
       height = 5,          
       dpi = 300)

# =============================================================================
# 2. TRACE PLOT: eta
# =============================================================================

eta_trace <- map_dfr(seq_along(chains$lcpa$eta), function(iter) {
  mat <- chains$lcpa$eta[[iter]]
  as.data.frame.table(mat, responseName="value") %>%
    mutate(iteration = iter)
})

if(ncol(eta_trace) == 6) {
  colnames(eta_trace)[1:4] <- c("Stage","Time","Profile","Group")
} else {
  colnames(eta_trace)[1:3] <- c("Stage","Time","Profile")
}

eta_trace <- eta_trace %>%
  mutate(
    Stage   = gsub("Stage\\.", "S", as.character(Stage)),
    Time    = gsub("Time\\.", "T",  as.character(Time)),
    Profile = gsub("Profile\\.", "P", as.character(Profile)),
    Group   = gsub("Group\\.", "G", as.character(Var5)),
    label   = paste0(Profile, "-", Stage)
  ) %>%
  select(-Var4, -Var5)

eta_trace_t1 <- eta_trace %>% filter(Time == "T1")
ggplot(eta_trace_t1, aes(x=iteration, y=value, color=Stage)) +
  geom_line(alpha=0.7, linewidth=0.5) +
  facet_grid(Group ~ Profile) +
  labs(title=paste("Trace Plot: Eta Trajectories (Time.1, N=",N,")",sep=""), 
       x="Iteration", y="Probability", color="Stage") +
  theme_minimal(base_size=11) +
  theme(strip.text=element_text(face="bold"))

ggsave(paste("SAN_50000/Diagnostics/Trace_plot","_eta1_N",N,".png",sep=""),
       width = 15,           
       height = 5,          
       dpi = 300)

eta_trace_t2 <- eta_trace %>% filter(Time == "T2")
p2<-ggplot(eta_trace_t2, aes(x=iteration, y=value, color=Stage)) +
  geom_line(alpha=0.7, linewidth=0.5) +
  facet_grid(Group ~ Profile) +
  labs(title=paste("Trace Plot: Eta Trajectories (Time.2, N=",N,")",sep=""),
       x="Iteration", y="Probability", color="Stage") +
  theme_minimal(base_size=11) +
  theme(strip.text=element_text(face="bold"))
ggsave(paste("SAN_50000/Diagnostics/Trace_plot","_eta2_N",N,".png",sep=""),
       plot = p2,
       width = 15,           
       height = 5,          
       dpi = 300)

eta_trace_t3 <- eta_trace %>% filter(Time == "T3")
p3<-ggplot(eta_trace_t3, aes(x=iteration, y=value, color=Stage)) +
  geom_line(alpha=0.7, linewidth=0.5) +
  facet_grid(Group ~ Profile) +
  labs(title=paste("Trace Plot: Eta Trajectories (Time.3, N=",N,")",sep=""),
       x="Iteration", y="Probability", color="Stage") +
  theme_minimal(base_size=11) +
  theme(strip.text=element_text(face="bold"))
ggsave(paste("SAN_50000/Diagnostics/Trace_plot","_eta3_N",N,".png",sep=""),
       plot = p3,
       width = 15,           
       height = 5,          
       dpi = 300)

# =============================================================================
# 3. TRACE PLOT: big.rho
# =============================================================================

rho_trace_raw <- map_dfr(seq_along(chains$lcpa$big.rho), function(iter) {
  mat <- chains$lcpa$big.rho[[iter]]
  as.data.frame.table(mat, responseName="value") %>%
    mutate(iteration = iter)
})

rho_trace <- rho_trace_raw %>%
  rename(Item=Var1, Category=Var2, Stage=Var3, Time=Var4, Drop=Var5, Group=Var6) %>%
  filter(as.character(Time) == "Time.1",
         as.character(Category) == "B") %>%  # B = Kategorie 2
  mutate(
    Item  = gsub("sItem\\.", "Item ", as.character(Item)),
    Stage = gsub("Stage\\.", "S",     as.character(Stage)),
    Group = gsub("Group\\.", "Group ", as.character(Group))
  ) %>%
  select(-Drop)


p4<-ggplot(rho_trace, aes(x=iteration, y=value, color=Item,
                      group=interaction(Item, Stage, Group))) +
  geom_line(alpha=0.5, linewidth=0.4) +
  facet_grid(Group ~ Stage) +
  scale_y_continuous(limits=c(0,1)) +
  labs(
    title    = paste("Trace Plot: Item Response Probabilities (big.rho, N=",N,")",sep=""),
    subtitle = "P(Item = 2) per Stage and Group",
    x        = "Iteration", y = "P(Item = 2)", color = "Item"
  ) +
  scale_color_brewer(palette="Set2") +
  theme_minimal(base_size=12) +
  theme(strip.text=element_text(face="bold"),
        legend.position="bottom")
ggsave(paste("SAN_50000/Diagnostics/Trace_plot_rho","_N",N,".png",sep=""),
       plot = p4,
       width = 15,           
       height = 5,          
       dpi = 300)

