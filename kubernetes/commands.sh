# Connects to psql
kubectl run -it --rm --image=dencold/pgcli --restart=Never pgcli -- postgres://nwreads_server:password@psql/nwreads
# Install istio for routing
istioctl install --set addonComponents.grafana.enabled=true
# get grafana url
kubectl -n prometheus-operator get pods | grep prometheus-operator-grafana<br>
# port forward grafana url
kubectl port-forward {FROM_LAST_COMMANd} -n prometheus-operator 8081:3000
# restart server pod
kubectl rollout restart deployment server
# connection string
postgres://nwreads_server:password@psql/nwreads
psql postgres://nwreads_server:password@psql/nwreads < dump.sql &> output.txt
pgcli postgres://nwreads_server:password@psql/nwreads
# Secret for ackee password 
kubectl create secret generic ackee --from-file=./ackee-password.txt
